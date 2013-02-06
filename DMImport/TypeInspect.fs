module TypeInspect

open System
open System.Reflection

let mutable sourceAssembly = ""
let mutable namespaceFilter = ""

let getName (t: Type) = 
    if t.IsGenericType then 
        let index = t.Name.IndexOf '`'
        let name = t.Name.Substring(0, index)
        let args = System.String.Join(", ", t.GetGenericArguments())
        name + "<" + args + ">"
    else t.Name

let types = lazy (
    let assembly = Assembly.LoadFile sourceAssembly
    assembly.GetExportedTypes() 
        |> Array.filter (fun t -> t.IsGenericType = false && t.IsInterface = false && (if namespaceFilter = null then true else t.Namespace.Contains(namespaceFilter)))
        |> Array.toList)

let typesMap = lazy (types.Value |> List.map (fun t -> (getName t, t)) |> Map.ofList)

let getProperties (t: Type) f = 
        t.GetProperties() 
        |> List.ofArray 
        |> List.filter f

let simpleProperties = lazy (
    types.Value 
        |> List.map (fun t ->
            let props = getProperties t (fun p -> 
                p.CanRead && 
                p.CanWrite && 
                p.PropertyType.IsGenericType = false && 
                p.PropertyType.Namespace.StartsWith("System"))
            (getName t, props))
        |> Map.ofList)

let inspectTypes a n =
    sourceAssembly <- a
    namespaceFilter <- n