module App

open Elmish
open Elmish.React

open Feliz
open Fetch
open Thoth.Fetch
open Thoth.Json



 

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

type ClaimResult = {clientPrincipal:ClientPrincipal}

module ClaimResult = 
    let decoder : Decoder<ClaimResult> =
        Decode.object (fun get ->
        { clientPrincipal = get.Required.Field "clientPrincipal" ClientPrincipal.decoder
          }
    )

type State =
    { Count: int
      Message: string option
      Fetching: bool }

type Msg =
    | Increment
    | FetchingMessage
    | MessageReceived of string
    | MessageError of exn
    

let init() =
    { Count = 0
      Message = None
      Fetching = false },
    Cmd.none


let update msg model =
    match msg with
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | FetchingMessage ->
        let getMessage () =
            promise {
                let! message =
                    Fetch.get(
                        "/.auth/me",
                        headers = [ HttpRequestHeaders.Accept "application/json" ],
                        decoder = ClaimResult.decoder
                    )

                return message.clientPrincipal.UserDetails
            }

        { model with Fetching = true }, Cmd.OfPromise.either getMessage () MessageReceived MessageError
    | MessageReceived message ->
        { model with
              Message = Some message
              Fetching = false },
        Cmd.none
    | MessageError err ->
        let xx = sprintf "%O" err 

        { model with
              Fetching = false
              Message = Some xx },
        Cmd.none


let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
    ]
    Html.button [ 
      prop.onClick(fun _ -> dispatch FetchingMessage)
      prop.text "Get a message from the server"
    ]


    match (state.Fetching, state.Message) with
    | (false, None) -> ()
    | (false, Some msg) -> Html.p msg
    | (true, _) -> Html.p "Fetching from server..."


    Html.h1 state.Count

    Html.a [
        prop.text "Logout"
        prop.href ("/.auth/logout")
        prop.style [ style.margin 5 ]
      ]
    
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run