module ClaudeAgentSdk.Transport

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open FsToolkit.ErrorHandling
open ClaudeAgentSdk

let private defaultMaxBufferSize = 1024 * 1024  // 1MB

// ============================================================================
// CLI Discovery
// ============================================================================

let private findBundledCli () : string option =
    // Check for bundled CLI in package directory
    let assemblyDir = Path.GetDirectoryName(typeof<SdkError>.Assembly.Location)
    let cliName = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "claude.exe" else "claude"
    let bundledPath = Path.Combine(assemblyDir, "_bundled", cliName)
    if File.Exists(bundledPath) then Some bundledPath else None

let private findInPath (name: string) : string option =
    let pathEnv = Environment.GetEnvironmentVariable("PATH")
    if String.IsNullOrEmpty(pathEnv) then None
    else
        let separator = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ';' else ':'
        pathEnv.Split(separator)
        |> Array.tryPick (fun dir ->
            let fullPath = Path.Combine(dir, name)
            if File.Exists(fullPath) then Some fullPath else None)

let private findSystemCli () : string option =
    let cliName = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "claude.exe" else "claude"

    // Try PATH first
    findInPath cliName
    |> Option.orElseWith (fun () ->
        // Try common installation locations
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        let locations = [
            Path.Combine(home, ".npm-global", "bin", "claude")
            "/usr/local/bin/claude"
            Path.Combine(home, ".local", "bin", "claude")
            Path.Combine(home, "node_modules", ".bin", "claude")
            Path.Combine(home, ".yarn", "bin", "claude")
            Path.Combine(home, ".claude", "local", "claude")
        ]
        locations |> List.tryFind File.Exists)

/// Find the Claude CLI binary
let findCli (explicitPath: string option) : Result<string, SdkError> =
    match explicitPath with
    | Some p when File.Exists p -> Ok p
    | Some p -> Error (CliNotFound $"CLI not found at: {p}")
    | None ->
        findBundledCli ()
        |> Option.orElseWith findSystemCli
        |> function
            | Some path -> Ok path
            | None -> Error (CliNotFound "Claude Code not found. Install with: npm install -g @anthropic-ai/claude-code")

// ============================================================================
// Build CLI Arguments
// ============================================================================

/// Build CLI arguments from Options
let buildArgs (options: Options) (isStreaming: bool) : string list =
    let args = ResizeArray<string>()

    args.AddRange(["--output-format"; "stream-json"; "--verbose"])

    // System prompt
    match options.SystemPrompt with
    | None ->
        args.AddRange(["--system-prompt"; ""])
    | Some (SystemPromptText text) ->
        args.AddRange(["--system-prompt"; text])
    | Some (SystemPromptPresetConfig (ClaudeCodePreset appendOpt)) ->
        match appendOpt with
        | Some append -> args.AddRange(["--append-system-prompt"; append])
        | None -> ()

    // Tools
    match options.Tools with
    | None -> ()
    | Some (ToolsList []) ->
        args.AddRange(["--tools"; ""])
    | Some (ToolsList tools) ->
        args.AddRange(["--tools"; String.Join(",", tools)])
    | Some (ToolsPresetConfig ClaudeCodeToolsPreset) ->
        args.AddRange(["--tools"; "default"])

    // Allowed/disallowed tools with auto-allow support
    let effectiveAllowedTools =
        match options.ToolAllowMode with
        | ManualControl -> options.AllowedTools
        | AutoAllowMcp ->
            let mcpTools = Options.enumerateMcpTools options.McpServers
            List.distinct (mcpTools @ options.AllowedTools)

    if not (List.isEmpty effectiveAllowedTools) then
        args.AddRange(["--allowedTools"; String.Join(",", effectiveAllowedTools)])

    if not (List.isEmpty options.DisallowedTools) then
        args.AddRange(["--disallowedTools"; String.Join(",", options.DisallowedTools)])

    // Max turns and budget
    match options.MaxTurns with
    | Some turns -> args.AddRange(["--max-turns"; string turns])
    | None -> ()

    match options.MaxBudgetUsd with
    | Some budget -> args.AddRange(["--max-budget-usd"; string budget])
    | None -> ()

    // Model
    match options.Model with
    | Some model -> args.AddRange(["--model"; model])
    | None -> ()

    match options.FallbackModel with
    | Some model -> args.AddRange(["--fallback-model"; model])
    | None -> ()

    // Permission mode
    match options.PermissionMode with
    | Some Default -> args.AddRange(["--permission-mode"; "default"])
    | Some AcceptEdits -> args.AddRange(["--permission-mode"; "acceptEdits"])
    | Some Plan -> args.AddRange(["--permission-mode"; "plan"])
    | Some BypassPermissions -> args.AddRange(["--permission-mode"; "bypassPermissions"])
    | None -> ()

    match options.PermissionPromptToolName with
    | Some name -> args.AddRange(["--permission-prompt-tool"; name])
    | None -> ()

    // Session management
    if options.ContinueConversation then
        args.Add("--continue")

    match options.Resume with
    | Some sessionId -> args.AddRange(["--resume"; sessionId])
    | None -> ()

    if options.ForkSession then
        args.Add("--fork-session")

    // Settings
    match options.Settings with
    | Some settings -> args.AddRange(["--settings"; settings])
    | None -> ()

    // Add directories
    for dir in options.AddDirs do
        args.AddRange(["--add-dir"; dir])

    // MCP servers
    if not (Map.isEmpty options.McpServers) then
        let serversJson =
            options.McpServers
            |> Map.toList
            |> List.map (fun (name, server) ->
                let serverJson =
                    match server with
                    | StdioServer (cmd, serverArgs, env) ->
                        let envPairs = env |> Map.toList |> List.map (fun (k, v) -> sprintf "\"%s\":\"%s\"" k v) |> String.concat ","
                        let argsList = serverArgs |> List.map (sprintf "\"%s\"") |> String.concat ","
                        sprintf """{"command":"%s","args":[%s],"env":{%s}}""" cmd argsList envPairs
                    | SseServer (url, headers) ->
                        let headerPairs = headers |> Map.toList |> List.map (fun (k, v) -> sprintf "\"%s\":\"%s\"" k v) |> String.concat ","
                        sprintf """{"type":"sse","url":"%s","headers":{%s}}""" url headerPairs
                    | HttpServer (url, headers) ->
                        let headerPairs = headers |> Map.toList |> List.map (fun (k, v) -> sprintf "\"%s\":\"%s\"" k v) |> String.concat ","
                        sprintf """{"type":"http","url":"%s","headers":{%s}}""" url headerPairs
                    | SdkServer (sdkName, _) ->
                        sprintf """{"type":"sdk","name":"%s"}""" sdkName
                sprintf "\"%s\":%s" name serverJson)
            |> String.concat ","
        let mcpConfig = sprintf """{"mcpServers":{%s}}""" serversJson
        args.AddRange(["--mcp-config"; mcpConfig])

    // Agents
    if not (Map.isEmpty options.Agents) then
        let agentsJson =
            options.Agents
            |> Map.toList
            |> List.map (fun (name, agent) ->
                let toolsStr =
                    match agent.Tools with
                    | Some tools -> sprintf ",\"tools\":[%s]" (tools |> List.map (sprintf "\"%s\"") |> String.concat ",")
                    | None -> ""
                let modelStr =
                    match agent.Model with
                    | Some Sonnet -> ",\"model\":\"sonnet\""
                    | Some Opus -> ",\"model\":\"opus\""
                    | Some Haiku -> ",\"model\":\"haiku\""
                    | Some Inherit -> ",\"model\":\"inherit\""
                    | None -> ""
                sprintf "\"%s\":{\"description\":\"%s\",\"prompt\":\"%s\"%s%s}"
                    name
                    (agent.Description.Replace("\"", "\\\""))
                    (agent.Prompt.Replace("\"", "\\\""))
                    toolsStr
                    modelStr)
            |> String.concat ","
        args.AddRange(["--agents"; sprintf "{%s}" agentsJson])

    // Setting sources
    match options.SettingSources with
    | Some sources ->
        let sourcesStr = sources |> List.map Json.Encode.settingSource |> String.concat ","
        args.AddRange(["--setting-sources"; sourcesStr])
    | None -> ()

    // Extra args
    for KeyValue(flag, value) in options.ExtraArgs do
        match value with
        | Some v -> args.AddRange([$"--{flag}"; v])
        | None -> args.Add($"--{flag}")

    // Max thinking tokens
    match options.MaxThinkingTokens with
    | Some tokens -> args.AddRange(["--max-thinking-tokens"; string tokens])
    | None -> ()

    // Output format (structured output)
    match options.OutputFormat with
    | Some schema ->
        let jsonSchema = Json.Encode.schema schema |> Thoth.Json.Net.Encode.toString 0
        args.AddRange(["--json-schema"; jsonSchema])
    | None -> ()

    // Include partial messages
    if options.IncludePartialMessages then
        args.Add("--include-partial-messages")

    // Input mode
    if isStreaming then
        args.AddRange(["--input-format"; "stream-json"])

    args |> Seq.toList

// ============================================================================
// Subprocess Management
// ============================================================================

/// Spawn a subprocess and return a Connection
let spawn (cliPath: string) (args: string list) (env: Map<string, string>) (cwd: string option)
    : Result<Connection, SdkError> =
    try
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- cliPath
        for arg in args do
            startInfo.ArgumentList.Add(arg)
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.CreateNoWindow <- true

        // Set working directory
        match cwd with
        | Some dir -> startInfo.WorkingDirectory <- dir
        | None -> ()

        // Set environment variables
        startInfo.Environment["CLAUDE_CODE_ENTRYPOINT"] <- "sdk-fsharp"
        for KeyValue(key, value) in env do
            startInfo.Environment[key] <- value

        let proc = Process.Start(startInfo)
        if proc = null then
            Error (ConnectionFailed "Failed to start process")
        else
            Ok {
                Process = proc
                Stdin = proc.StandardInput
                Stdout = proc.StandardOutput
                Stderr = Some proc.StandardError
            }
    with ex ->
        Error (ConnectionFailed $"Failed to spawn process: {ex.Message}")

/// Write a line to the connection's stdin
let write (line: string) (conn: Connection) : Task<Result<unit, SdkError>> = taskResult {
    try
        do! conn.Stdin.WriteLineAsync(line)
        do! conn.Stdin.FlushAsync()
    with ex ->
        return! Error (ProtocolError $"Failed to write: {ex.Message}")
}

/// Read lines from stdout as a TaskSeq
let read (conn: Connection) : IAsyncEnumerable<string> = taskSeq {
    let mutable continueReading = true
    while continueReading && not conn.Process.HasExited do
        let! line = conn.Stdout.ReadLineAsync()
        if line <> null then
            yield line
        else
            continueReading <- false
}

/// Read lines with JSON buffering (handles multi-line JSON)
let readWithBuffer (conn: Connection) (maxBufferSize: int option) : IAsyncEnumerable<string> = taskSeq {
    let maxSize = maxBufferSize |> Option.defaultValue defaultMaxBufferSize
    let buffer = System.Text.StringBuilder()

    // Discard stderr (uncomment for debugging)
    // let stderrTask = task {
    //     match conn.Stderr with
    //     | Some stderr ->
    //         try
    //             while not conn.Process.HasExited do
    //                 let! line = stderr.ReadLineAsync()
    //                 if line <> null then
    //                     System.Console.ForegroundColor <- System.ConsoleColor.DarkYellow
    //                     System.Console.WriteLine(sprintf "[STDERR] %s" line)
    //                     System.Console.ResetColor()
    //         with _ -> ()
    //     | None -> ()
    // }
    // let _ = stderrTask // Fire and forget

    let mutable continueReading = true
    while continueReading && not conn.Process.HasExited do
        let! line = conn.Stdout.ReadLineAsync()
        if line <> null then
            // Optionally show raw lines for debugging (disabled by default)
            // System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
            // let preview = if line.Length > 500 then line.Substring(0, 500) + "..." else line
            // System.Console.WriteLine(sprintf "[RAW] %s" preview)
            // System.Console.ResetColor()

            buffer.Append(line) |> ignore

            if buffer.Length > maxSize then
                buffer.Clear() |> ignore
                // Skip oversized message
            else
                let json = buffer.ToString()
                // Try to parse as complete JSON
                try
                    let doc = System.Text.Json.JsonDocument.Parse(json)
                    doc.Dispose()
                    yield json
                    buffer.Clear() |> ignore
                with
                | :? System.Text.Json.JsonException ->
                    // Incomplete JSON, continue buffering
                    ()
        else
            continueReading <- false
}

/// Close the connection
let close (conn: Connection) : Task<unit> = task {
    try
        conn.Stdin.Close()
        conn.Stdout.Close()
        match conn.Stderr with
        | Some stderr -> stderr.Close()
        | None -> ()

        if not conn.Process.HasExited then
            conn.Process.Kill()
            do! conn.Process.WaitForExitAsync()
    with _ ->
        ()
}

/// End stdin (signal EOF without closing)
let endInput (conn: Connection) : Task<unit> = task {
    try
        conn.Stdin.Close()
    with _ ->
        ()
}
