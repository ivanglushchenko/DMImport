module DMImport

open System
open System.Xml.Linq
open System.Reflection
open TypeInspect

let xname (name: string) =
    if name.Contains(":") then
        let index = name.LastIndexOf(':')
        XName.Get(name.Substring(index + 1), name.Substring(0, index))
    else XName.Get(name, "http://schemas.microsoft.com/ado/2009/11/edmx")

let getNextShapePos i = 
    let getLevelCount i =
        match i with
        | 0 -> 0
        | i -> i * i - (i - 1) * (i - 1)
    let getIndexLevel i =
        let rec checkNextLevel l sum =
            let nextLevel = sum + getLevelCount (l + 1)
            if sum <= i && i < nextLevel then (l + 1, i - sum) else checkNextLevel (l + 1) nextLevel
        checkNextLevel 0 0
    let (level, offset) = getIndexLevel i
    let (x, y) = if offset < level then (level, offset + 1) else (2 * level - offset - 1, level)
    (float(x - 1) * 3.2, float(y - 1) * 2.0)

let getDescendants name (element: XElement) = element.Descendants(xname name)
let getDescendant name (element: XElement) = element |> getDescendants name |> Seq.head
let getAttr name (element: XElement) = element.Attribute(xname (":" + name)).Value
let newElem name (content: Object seq) = new XElement(XName.Get(name, "http://schemas.microsoft.com/ado/2009/11/edm"), content) :> Object
let newAttr name value = new XAttribute(XName.op_Implicit name, value)   :> Object

let getTypeStr (t: Type) = 
    if t.IsGenericType then 
        let index = t.Name.IndexOf '`'
        let name = t.Name.Substring(0, index)
        let args = System.String.Join(", ", t.GetGenericArguments())
        name + "<" + args + ">"
    else t.Name

let newEntSet name entityType = newElem "EntitySet" [ newAttr "Name" name; newAttr "EntityType" entityType ]

let newEntShape name ns (x, y) =
    let content = [ newAttr "EntityType" (ns + "." + name)
                    newAttr "Width" "2"
                    newAttr "PointX" (x.ToString())
                    newAttr "PointY" (y.ToString())
                    newAttr "IsExpanded" "false" ]
    new XElement(XName.Get("EntityTypeShape", "http://schemas.microsoft.com/ado/2009/11/edmx"), content)

let importDomainModel (sourceAssemblyPath: string) (targetModelPath: string) startingNamespace =
    inspectTypes sourceAssemblyPath startingNamespace

    let types = typesMap.Value

    let newEntType (entityType: Type) =
        let newProp n typeName isNullable = 
            newElem "Property"
                [ newAttr "Type" typeName
                  newAttr "Name" n
                  newAttr "Nullable" isNullable ]
        let newNavProp (pi: PropertyInfo) = 
            newElem "NavigationProperty" 
                [ newAttr "Name" pi.Name
                  newAttr "FromRole" entityType.Name
                  newAttr "ToRole" (pi.PropertyType.GetGenericArguments().[0].Name) ]
        let content = [ newAttr "Name" (entityType.Name);
                        newElem "Key" 
                            [ newElem "PropertyRef" 
                                [ newAttr "Name" "Id" ] ];
                            newProp "Id" "Int32" "false" ]
        let simpleProps = 
            entityType.GetProperties() 
            |> List.ofArray 
            |> List.filter (fun p -> p.CanRead && p.CanWrite && p.Name <> "Id" && p.PropertyType.IsGenericType = false && p.PropertyType.Namespace.StartsWith("System"))
            |> List.map (fun p -> newProp (p.Name) (getTypeStr p.PropertyType) "true")
        let navigationProps =
            entityType.GetProperties() 
            |> List.ofArray 
            |> List.filter (fun p -> p.CanRead && p.CanWrite && p.PropertyType.IsGenericType && types.ContainsKey(p.PropertyType.GetGenericArguments().[0].Name))
            |> List.map (fun p -> newNavProp p)
        newElem "EntityType" (content @ simpleProps)// @ navigationProps)

    let docModel = targetModelPath |> XDocument.Load
    let schema = docModel.Root |> getDescendant "ConceptualModels" |> getDescendant "http://schemas.microsoft.com/ado/2009/11/edm:Schema"
    let ns = schema |> getAttr "Namespace"
    let container =  schema |> getDescendant "http://schemas.microsoft.com/ado/2009/11/edm:EntityContainer"

    schema.RemoveNodes()
    container.RemoveNodes()
    
    schema.Add(container)
    container.Add(types |> Map.toSeq |> Seq.map snd |> Seq.map (fun t -> newEntSet (t.Name) (ns + "." + t.Name)))
    schema.Add(types |> Map.toSeq |> Seq.map (snd >> newEntType))

    docModel.Save(targetModelPath)

    let diagramPath = targetModelPath + ".diagram"
    let docDiagram = diagramPath |> XDocument.Load
    let diagram = docDiagram.Root |> getDescendant "Diagram"
    diagram.RemoveNodes()
    diagram.Add(types |> Map.toSeq |> Seq.mapi (fun i (n, t) -> newEntShape (t.Name) ns (getNextShapePos i)))
    docDiagram.Save(diagramPath)
    ()