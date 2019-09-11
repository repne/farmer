namespace Farmer

/// A Value is a reference to some value used by the ARM template. This can be a literal value,
/// a reference to a parameter, or a reference to a variable.
type Value =
    | Literal of string
    | Parameter of string
    | Variable of string
    member this.AsString =
        match this with
        | Literal l -> l
        | Parameter p -> sprintf "parameters('%s')" p
        | Variable v -> sprintf "variables('%s')" v
    member this.QuotedValue =
        match this with
        | Literal l -> sprintf "'%s'" l
        | Parameter p -> sprintf "parameters('%s')" p
        | Variable v -> sprintf "variables('%s')" v
    member this.Command =
        match this with
        | Literal l ->
            l
        | Parameter _
        | Variable _ ->
            sprintf "[%s]" this.AsString

type ResourceName =
    | ResourceName of Value
    member this.Command =
        let (ResourceName v) = this
        v.Command

[<AutoOpen>]
module ExpressionBuilder =
    let private escaped = function
        | Literal x -> sprintf "'%s'" x
        | x -> x.AsString
    let toExpr = sprintf "[%s]"
    let command a b = sprintf "%s(%s)" a b |> toExpr
    let concat (elements:Value list) =
        elements
        |> List.map escaped
        |> String.concat ", "
        |> command "concat"
    let toLower =
        escaped >> command "toLower"

type ConsistencyPolicy = Eventual | ConsistentPrefix | Session | BoundedStaleness of maxStaleness:int * maxIntervalSeconds : int | Strong
type FailoverPolicy = NoFailover | AutoFailover of secondaryLocation:string | MultiMaster of secondaryLocation:string
type CosmosDbIndexKind = Hash | Range
type CosmosDbIndexDataType = Number | String

namespace Farmer.Internal

open Farmer

/// A type of ARM resource e.g. Microsoft.Web/serverfarms
type ResourceType =
    | ResourceType of path:string
    member this.Value =
        let (ResourceType path) = this
        path
/// A path to a specific ARM resource.
type ResourcePath =
    | ResourcePath of ResourceType * resourceName:ResourceName 
    member this.ResourceIdPath =
        let (ResourcePath(ResourceType path, ResourceName name)) = this
        sprintf "resourceId('%s', %s)" path name.QuotedValue

type Expressions =
    | Concat of Value list
    | ToLower of Value

type WebAppExtensions = AppInsightsExtension
type AppInsights =
    { Name : ResourceName 
      Location : Value
      LinkedWebsite: ResourceName }
type StorageAccount =
    { Name : ResourceName 
      Location : Value
      Sku : Value }
type WebApp =
    { Name : ResourceName 
      AppSettings : List<string * Value>
      Extensions : WebAppExtensions Set
      Dependencies : ResourceName list }
type ServerFarm =
    { Name : ResourceName 
      Location : Value
      Sku:Value
      WebApps : WebApp list }
type CosmosDbContainer =
    { Name : ResourceName
      PartitionKey :
        {| Paths : string list
           Kind : CosmosDbIndexKind |}
      IndexingPolicy :
        {| IncludedPaths :
            {| Path : string
               Indexes :
                {| Kind : CosmosDbIndexKind
                   DataType : CosmosDbIndexDataType |} list
            |} list
           ExcludedPaths : string list
        |}
    }

type CosmosDbSql =
    { Name : ResourceName
      Dependencies : ResourcePath list
      Throughput : Value
      Containers : CosmosDbContainer list }
type CosmosDbServer =
    { Name : ResourceName
      Location : Value
      ConsistencyPolicy : ConsistencyPolicy
      WriteModel : FailoverPolicy
      Databases : CosmosDbSql list }

module ResourcePath =
    let private makeResource x y = ResourcePath (x, y)
    let ServerFarm = ResourceType "Microsoft.Web/serverfarms"
    let WebSite = ResourceType "Microsoft.Web/sites"
    let CosmosDb = ResourceType "Microsoft.DocumentDB/databaseAccounts"
    let CosmosDbSql = ResourceType "Microsoft.DocumentDB/databaseAccounts/apis/databases"
    let CosmosDbSqlContainer = ResourceType "Microsoft.DocumentDb/databaseAccounts/apis/databases/containers"
    let StorageAccount = ResourceType "Microsoft.Storage/storageAccounts"
    let AppInsights = ResourceType "Microsoft.Insights/components"
    let makeServerFarm = makeResource ServerFarm
    let makeWebSite = makeResource WebSite
    let makeCosmosDb = makeResource CosmosDb
    let makeCosmosDbSql (accountName:ResourceName) (databaseName:ResourceName) = makeResource CosmosDbSql
    let makeCosmosDbSqlContainer = makeResource CosmosDbSqlContainer
    let makeStorageAccount = makeResource StorageAccount
    let makeAppInsights = makeResource AppInsights

namespace Farmer

type ArmTemplate =
    { Parameters : string list
      Variables : (string * string) list
      Outputs : (string * Value) list
      Resources : obj list }