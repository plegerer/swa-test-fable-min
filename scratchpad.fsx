#r "nuget: Thoth.Json.Net"
open System
open Thoth.Json.Net

module Types = 

    type UserRole =
        |Testrole
        |Anonymous
        |Authenticated
    module UserRole =
        let decoder : Decoder<UserRole> =
            Decode.string
            |> Decode.andThen (function
                | "testrole" -> Decode.succeed Testrole
                | "anonymous" -> Decode.succeed Anonymous
                | "authenticated" -> Decode.succeed Authenticated
                | invalid ->
                    Decode.fail $"%s{invalid} is not valid"
            )
        let decoderList : Decoder<UserRole list> =
            Decode.list decoder

    type ClientPrincipal ={
        IdentityProvider:string
        UserId:string
        UserDetails:string
        UserRoles: UserRole List
        }
    module ClientPrincipal =
        let decoder : Decoder<ClientPrincipal> =
            Decode.object (fun get ->
            { IdentityProvider = get.Required.Field "identityProvider" Decode.string
              UserId = get.Required.Field "userId" Decode.string
              UserDetails = get.Required.Field "userDetails" Decode.string
              UserRoles = get.Required.Field "userRoles" UserRole.decoderList
              }
        )

open Types
open System.Text
            
let testObj="{
\"clientPrincipal\": {
    \"identityProvider\": \"aad\",
    \"userId\": \"fbd02ca1a366452bacc07282df44244d\",
    \"userDetails\": \"paul.legerer@gmx.at\",
    \"userRoles\": [
      \"testrole\",
      \"anonymous\",
      \"authenticated\"
    ]
  }
}"

