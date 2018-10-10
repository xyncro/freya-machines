[xml]$doc = Get-Content .\src\Directory.Build.props
$version = $doc.Project.PropertyGroup.VersionPrefix # the version under development, update after a release
$versionSuffix = '-build.0' # manually incremented for local builds

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

if ($env:appveyor){
    $versionSuffix = '-build.' + $env:appveyor_build_number
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
        $versionSuffix = ''
    }
    Update-AppveyorBuild -Version "$version$versionSuffix"
}

dotnet build -c Release Freya.Machines.sln /p:Version=$version$versionSuffix
dotnet test --no-build -c Release tests/Freya.Machines.Http.Tests/Freya.Machines.Http.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Machines.Http.Hopac.Tests/Freya.Machines.Http.Hopac.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Machines.Http.Cors.Tests/Freya.Machines.Http.Cors.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Machines.Http.Cors.Hopac.Tests/Freya.Machines.Http.Cors.Hopac.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Machines.Http.Patch.Tests/Freya.Machines.Http.Patch.Tests.fsproj
dotnet test --no-build -c Release tests/Freya.Machines.Http.Patch.Hopac.Tests/Freya.Machines.Http.Patch.Hopac.Tests.fsproj
dotnet pack --no-build -c Release src/Freya.Machines /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Hopac /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http.Hopac /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http.Cors /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http.Cors.Hopac /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http.Patch /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release src/Freya.Machines.Http.Patch.Hopac /p:Version=$version$versionSuffix -o $psscriptroot/bin
