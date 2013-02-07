module TypeInspect

open System
open System.Reflection

let mutable sourceAssembly = ""
let mutable namespaceFilter = ""

type Relation = { name: string; relType: string; roleFrom: string; roleTo: string; multiplicity: string; hasNavProp: bool }
type Association = Relation * Relation
    
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
        |> Array.filter (fun t -> t.IsEnum = false && t.IsGenericType = false && t.IsInterface = false && (if namespaceFilter = null then true else t.Namespace.Contains(namespaceFilter)))
        |> Array.toList)

let typesMap = lazy (types.Value |> List.map (fun t -> (getName t, t)) |> Map.ofList)

let getReadWriteProperties (t: Type) f = 
        t.GetProperties() 
            |> List.ofArray 
            |> List.filter (fun p -> p.CanRead && p.CanWrite && f p)

let getAllReadWriteProperties t = getReadWriteProperties t (fun t -> true)

let (|SimpleProperty|_|) (p: PropertyInfo) = if p.CanRead && p.CanWrite && p.PropertyType.IsGenericType = false && p.PropertyType.Namespace.StartsWith("System") then Some(p) else None
let (|NavigationChildren|_|) (p: PropertyInfo) = if p.CanRead && p.CanWrite && p.PropertyType.IsGenericType && p.PropertyType.GetGenericArguments().Length = 1 && (p.PropertyType.GetGenericArguments().[0] |> getName |> typesMap.Value.ContainsKey) then Some(p.PropertyType.GetGenericArguments().[0]) else None
let (|NavigationParent|_|) (p: PropertyInfo) = if p.CanRead && p.CanWrite && p.PropertyType.IsGenericType = false && p.PropertyType |> getName |> typesMap.Value.ContainsKey then Some(p.PropertyType) else None

let simpleProperties = lazy (
    types.Value 
        |> List.map (fun t ->
            let props = getReadWriteProperties t (fun p -> 
                p.PropertyType.IsGenericType = false && 
                p.PropertyType.Namespace.StartsWith("System"))
            (getName t, props))
        |> Map.ofList)

let getSimpleProperties t = 
    let name = getName t
    if simpleProperties.Value.ContainsKey name then simpleProperties.Value.[name] else []

let evalAssociations () =
    let propSignature (p: PropertyInfo) = getName p.DeclaringType + ":" + p.Name
    let allProperties = types.Value |> List.collect (fun t -> getAllReadWriteProperties t)

    let allPropertiesMap =
        allProperties
        |> Seq.groupBy (fun t -> getName t.DeclaringType)
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map (fun t -> (t.Name, t)) |> Map.ofSeq))
        |> Map.ofSeq

    let getProperties t = 
        let name = getName t
        if allPropertiesMap.ContainsKey name then allPropertiesMap.[name] |> Map.toList |> List.map snd else []

    let collectAssociations (p: PropertyInfo) (roles: Set<string>) = 
        let parentChildPairs =
            match p with
            | NavigationChildren cType ->
                let matches = 
                    getProperties cType 
                        |> List.filter (fun t -> 
                            match t with
                            | NavigationParent pType -> pType = p.DeclaringType
                            | _ -> false)
                        |> List.map (fun parent -> (p, parent))
                if matches.IsEmpty then [(p, null)]
                else matches
            | _ -> []
        
        let rec getAvailableRoleName (name: string) roles =
            if roles |> Set.contains name then 
                let nextName =
                    let lastChar = name.[name.Length - 1]
                    if Char.IsDigit lastChar then
                        if lastChar = '9' then name.Substring(0, name.Length - 1) + "10"
                        else name.Substring(0, name.Length - 1) + (System.Int32.Parse(lastChar.ToString()) + 1).ToString()
                    else name + "1"
                getAvailableRoleName nextName roles
            else (name, roles.Add name)

        let foldPairs (associations, roles) (p: PropertyInfo, parent: PropertyInfo) =
            let childClass = p.PropertyType.GetGenericArguments().[0]
            let childClassName = p.PropertyType.GetGenericArguments().[0].Name
            let (rn1, roles) = getAvailableRoleName childClassName roles
            let (rn2, roles) = getAvailableRoleName p.DeclaringType.Name roles
            let r1 = { name = (if parent = null then childClassName else parent.Name); relType = childClassName; roleFrom = rn1; roleTo = rn2; multiplicity = "*"; hasNavProp = parent <> null }
            let r2 = { name = p.Name; relType = p.DeclaringType.Name; roleFrom = rn2; roleTo = rn1; multiplicity = "1"; hasNavProp = true }
            ((r1, r2) :: associations, roles)

        parentChildPairs |> List.fold foldPairs (List.empty, roles)

    let rec loop props associations roles =
        match props with
        | hd :: tl -> 
            let (newAssoc, newRoles) = collectAssociations hd roles
            loop tl (newAssoc @ associations) newRoles
        | _        -> associations

    loop allProperties List.empty Set.empty

let associations = lazy (evalAssociations ())

let getAssociationName r1 r2 = if r1.multiplicity = "1" then r1.roleFrom + r2.roleFrom else r2.roleFrom + r1.roleFrom

let evalNavigationPropeties () = 
    associations.Value
        |> List.map (fun (r1, r2) -> 
            let t1 = if r1.hasNavProp then [(r1.relType, (getAssociationName r1 r2, r1))] else []
            let t2 = if r2.hasNavProp then [(r2.relType, (getAssociationName r1 r2, r2))] else []
            t1 @ t2)
        |> List.collect (fun t -> t)
        |> List.filter (fun t -> true)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> List.ofSeq))
        |> Map.ofSeq


let navigationPropeties = lazy (evalNavigationPropeties ())

let getNavigationProperties t = 
    let name = getName t
    if navigationPropeties.Value.ContainsKey name then navigationPropeties.Value.[name] else []

let inspectTypes a n =
    sourceAssembly <- a
    namespaceFilter <- n
