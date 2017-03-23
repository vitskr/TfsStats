// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"

let testDir = "./tests/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.exe")
    ++ (buildDir + "/**/*.exe.config")
    ++ (buildDir + "/**/*.dll")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Test" (fun _ ->
    !!(buildDir @@ "*.Tests.dll")
        |> xUnit2 (fun p ->
            { p with HtmlOutputPath = Some (testDir @@ "results.html") })
)


// Build order
"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Deploy"

// start build
RunTargetOrDefault "Test"
