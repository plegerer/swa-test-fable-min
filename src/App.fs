module App

open Elmish
open Elmish.React
open Feliz
open Fetch
open Thoth.Fetch
open Feliz.Router

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
                    Fetch.get<unit, string> (
                        "/.auth/me",
                        headers = [ HttpRequestHeaders.Accept "application/json" ]
                    )

                return message
            }

        { model with Fetching = true }, Cmd.OfPromise.either getMessage () MessageReceived MessageError
    | MessageReceived message ->
        { model with
              Message = Some message
              Fetching = false },
        Cmd.none
    | MessageError err ->
        printfn "%O" err

        { model with
              Fetching = false
              Message = None },
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
        prop.text "Login"
        prop.href ("/.auth/login/aad")
        prop.style [ style.margin 5 ]
      ]
    
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run