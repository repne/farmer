[<AutoOpen>]
module Farmer.ArmBuilder
open System

module Helpers =
    /// Adapts a raw ArmResourceBuilder into a "full" ResourceBuilder that can be added as a resource to arm { } expressions.
    let asResourceBuilder (builder:ArmResourceBuilder) =
        let output : ResourceBuilder =
            fun (location:Location) -> [
                for resourceName, armObject in builder location do
                    NewResource
                        { new IResource with
                            member _.ResourceName = ResourceName resourceName
                            member _.ToArmObject() = armObject }
            ]
        output

type IResourceKey = string * ResourceName

/// Represents all configuration information to generate an ARM template.
type ArmConfig =
    { Parameters : string Set
      Outputs : Map<string, string>
      Location : Location
      Resources : Map<IResourceKey, IResource> }

type ArmBuilder() =
    member __.Yield _ =
        { Parameters = Set.empty
          Outputs = Map.empty
          Resources = Map.empty
          Location = WestEurope }

    member __.Run (state:ArmConfig) =
        let resources =
            state.Resources
            |> Map.toList
            |> List.map snd
        let template =
            { Parameters = [
                for resource in resources do
                    match resource with
                    | :? IParameters as p -> yield! p.SecureParameters
                    | _ -> ()
              ] |> List.distinct
              Outputs = state.Outputs |> Map.toList
              Resources = resources }

        let postDeployTasks = [
            for resource in resources do
                match resource with
                | :? IPostDeploy as pd -> pd
                | _ -> ()
            ]

        { Location = state.Location
          Template = template
          PostDeployTasks = postDeployTasks }

    /// Creates an output value that will be returned by the ARM template.
    [<CustomOperation "output">]
    member __.Output (state, outputName, outputValue) : ArmConfig = { state with Outputs = state.Outputs.Add(outputName, outputValue) }
    member this.Output (state:ArmConfig, outputName:string, (ResourceName outputValue)) = this.Output(state, outputName, outputValue)
    member this.Output (state:ArmConfig, outputName:string, outputValue:ArmExpression) = this.Output(state, outputName, outputValue.Eval())
    member this.Output (state:ArmConfig, outputName:string, outputValue:string option) =
        match outputValue with
        | Some outputValue -> this.Output(state, outputName, outputValue)
        | None -> state
    member this.Output (state:ArmConfig, outputName:string, outputValue:ArmExpression option) =
        match outputValue with
        | Some outputValue -> this.Output(state, outputName, outputValue)
        | None -> state

    /// Sets the default location of all resources.
    [<CustomOperation "location">]
    member __.Location (state, location) : ArmConfig = { state with Location = location }

    /// Adds a single resource to the ARM template.
    [<CustomOperation "add_resource">]
    member this.AddResource (state:ArmConfig, builder:IResourceBuilder) =
        this.AddResource(state, builder.BuildResources)

    member __.AddResource(state:ArmConfig, builder:ResourceBuilder) =
        let resources =
            builder state.Location
            |> List.fold(fun (resources:Map<_,_>) action ->
                match action with
                | NewResource newResource ->
                    let key = (newResource.GetType().FullName, newResource.ResourceName)
                    if resources.ContainsKey key then printfn "Warning: An existing resource was found with the same type and name of '%O'. It will be replaced with the newer one." key
                    resources.Add(key, newResource)
                | MergeResource(name, resourceType, merger) ->
                    let key = resourceType.FullName, name
                    key
                    |> resources.TryFind
                    |> Option.map (fun value -> resources.Add(key, merger value))
                    |> Option.defaultWith (fun _ -> failwithf "Could not locate the parent resource ('%A'). Make sure you have correctly specified the name, and that it was added to the arm { } builder before this one." key)
                | NotSet ->
                    failwith "No parent resource name was set for this resource to link to.") state.Resources

        { state with Resources = resources }

    [<CustomOperation "add_resources">]
    /// Adds a sequence of resources to the ARM template.
    member this.AddResources (state:ArmConfig, builders:IResourceBuilder list) =
        builders
        |> List.fold(fun state builder -> this.AddResource(state, builder)) state

let arm = ArmBuilder()