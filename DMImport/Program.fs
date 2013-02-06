open DMImport

let printUsage =
    printf "Usage: DMImport.exe sourceAssemblyPath targetModelPath [startingNamespace]"

[<EntryPoint>]
let main argv = 
    match argv |> List.ofArray with
    | hd1 :: hd2 :: hd3 :: tl -> importDomainModel hd1 hd2 hd3
    | hd1 :: hd2 :: tl        -> importDomainModel hd1 hd2 null
    | _                       -> printUsage

    0