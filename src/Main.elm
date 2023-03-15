port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type alias Model =
    { draft : String
    , messages : List String
    }


type Msg
    = UpdateDraft String
    | SendMessage
    | ReceiveMessage String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draft = "", messages = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDraft newDraft ->
            ( { model | draft = newDraft }, Cmd.none )

        SendMessage ->
            ( { model | draft = "" }, sendMessage model.draft )

        ReceiveMessage message ->
            ( { model | messages = model.messages ++ [ message ] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver ReceiveMessage


view : Model -> Html Msg
view model =
    div []
        [ ul [] (List.map (\msg -> li [] [ text msg ]) model.messages)
        , input
            [ type_ "text"
            , onInput UpdateDraft
            , on "keydown" (ifIsEnter SendMessage)
            , value model.draft
            ]
            []
        , button [ onClick SendMessage ] [ text "Send" ]
        ]


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "Not an 'Enter'"
            )
