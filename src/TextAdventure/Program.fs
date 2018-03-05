module Domain =
    type Location = {
        longDescription: string
    }

    type PlayerCommand =
        | Look
        | Quit
    
    type Result =
        | Description of string
        | Quitting
    
    type NewGame<'GameState> = unit -> 'GameState * Result

    type PlayerTurn<'GameState> = 'GameState * PlayerCommand -> 'GameState * Result

    type Api<'GameState>  = {
        newGame : NewGame<'GameState>
        playerTurn : PlayerTurn<'GameState>
    }

module Implementation =
    open Domain

    type GameState = {
        locations: Location list
        playerLocation: Location
    }

    let newGame : NewGame<GameState> =
        fun () ->
            let state = {
                locations = [{longDescription = "Somewhere dark"}]
                playerLocation = {longDescription = "Somewhere dark"}
            }
            let result = Description <| "Welcome to TextAdventure.\n" + state.playerLocation.longDescription
            state, result
    
    let look state =
        state, Description state.playerLocation.longDescription
    
    let playerTurn : PlayerTurn<GameState> =
        fun (state, command) ->
            match command with
            | Look -> look state
            | Quit -> state, Quitting

    let api = {
        newGame = newGame
        playerTurn = playerTurn
    }

module Ui =
    open System
    open Domain

    let displayResult x =
        match x with
        | Description d -> printfn "%s" d
        | Quitting -> printfn "Bye bye!"

    let getPlayerInput () =
        match Console.ReadKey().Key with
        | ConsoleKey.L -> Look
        | _ -> Quit

open Domain
open Ui

let api = Implementation.api

let rec loop (state, result) =
    match result with
    | Quitting -> ()
    | _ ->
        displayResult result
        let command = getPlayerInput()
        loop <| api.playerTurn (state, command)

[<EntryPoint>]
let main argv =
    api.newGame ()
    |> loop

    0
