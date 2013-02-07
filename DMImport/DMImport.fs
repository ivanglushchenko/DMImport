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

let newEntSet name entityType = newElem "EntitySet" [ newAttr "Name" (name + "s"); newAttr "EntityType" entityType ]

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
    let docModel = targetModelPath |> XDocument.Load
    let schema = docModel.Root |> getDescendant "ConceptualModels" |> getDescendant "http://schemas.microsoft.com/ado/2009/11/edm:Schema"
    let ns = schema |> getAttr "Namespace"
    let container =  schema |> getDescendant "http://schemas.microsoft.com/ado/2009/11/edm:EntityContainer"

    let newEntType (entityType: Type) =
        let newProp n typeName isNullable = 
            newElem "Property"
                [ newAttr "Type" typeName
                  newAttr "Name" n
                  newAttr "Nullable" isNullable ]
        let newNavProp (n, rel) = 
            newElem "NavigationProperty" 
                [ newAttr "Name" rel.name
                  newAttr "Relationship" (ns + "." + n)
                  newAttr "FromRole" rel.roleFrom
                  newAttr "ToRole" rel.roleTo ]
        let content = [ newAttr "Name" (entityType.Name)
                        newElem "Key" 
                            [ newElem "PropertyRef" 
                                [ newAttr "Name" "Id" ] ] ]
        let simpleProps = 
            getSimpleProperties entityType 
                |> List.map (fun p -> newProp (p.Name) (getName p.PropertyType) (if p.Name = "Id" then "false" else "true"))
        let simpleProps =
            if getSimpleProperties entityType |> List.exists (fun p -> p.Name = "Id") then simpleProps
            else newProp "Id" "Int32" "false" :: simpleProps
        let navigationProperties = getNavigationProperties entityType |> List.map (fun p -> newNavProp p)
        newElem "EntityType" (content @ simpleProps @ navigationProperties)

    let newAssociationSet (r1, r2) = 
        let name = getAssociationName r1 r2
        newElem "AssociationSet"
            [ newAttr "Name" name
              newAttr "Association" (ns + "." + name)
              newElem "End"
                [ newAttr "Role" r1.roleFrom
                  newAttr "EntitySet" (r1.relType + "s") ]
              newElem "End"
                [ newAttr "Role" r2.roleFrom
                  newAttr "EntitySet" (r2.relType + "s") ]]

    let newAssociation (r1, r2) = 
        let name = getAssociationName r1 r2
        newElem "Association"
            [ newAttr "Name" name
              newElem "End"
                [ newAttr "Type" (ns + "." + r1.relType)
                  newAttr "Role" r1.roleFrom
                  newAttr "Multiplicity" r1.multiplicity ]
              newElem "End"
                [ newAttr "Type" (ns + "." + r2.relType)
                  newAttr "Role" r2.roleFrom
                  newAttr "Multiplicity" r2.multiplicity ]]

    let newAssociationConnector (r1, r2) = 
        let name = getAssociationName r1 r2
        newElem "AssociationConnector" [ newAttr "Association" (ns + "." + name) ]
                  
    schema.RemoveNodes()
    container.RemoveNodes()
    
    schema.Add(container)
    container.Add(types |> Map.toSeq |> Seq.map snd |> Seq.map (fun t -> newEntSet (t.Name) (ns + "." + t.Name)))
    container.Add(associations.Value |> Seq.map newAssociationSet)
    schema.Add(types |> Map.toSeq |> Seq.map (snd >> newEntType))
    schema.Add(associations.Value |> Seq.map newAssociation)

    docModel.Save(targetModelPath)

    let diagramPath = targetModelPath + ".diagram"
    let docDiagram = diagramPath |> XDocument.Load
    let diagram = docDiagram.Root |> getDescendant "Diagram"
    diagram.RemoveNodes()
    diagram.Add(types |> Map.toSeq |> Seq.mapi (fun i (n, t) -> newEntShape (t.Name) ns (getNextShapePos i)))
    diagram.Add(associations.Value |> Seq.map newAssociationConnector)
    docDiagram.Save(diagramPath)
    ()