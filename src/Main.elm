module Main exposing (..)

import Http
import Result
import Json.Encode as Encode
import Json.Decode as Decode
import Html exposing (Html, div, form, text, program, label)
import Html.Attributes exposing (style)
import Html.Events exposing (onSubmit)
import Material
import Material.Scheme as Scheme
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)
import Maybe exposing (..)

{-| CONSTANTS & DEFINITIONS

-}
userServiceUrl : String
userServiceUrl = "http://localhost:3000/api/AppUser/"

inputDefs : String -> String -> List (Textfield.Property m)
inputDefs value label =
    [ Textfield.floatingLabel
    , Textfield.value value
    , Textfield.label label
    ]

buttonDefs : List (Button.Property a)
buttonDefs =
    [ Button.ripple
    , Button.raised
    ]

{-| MODEL

-}
type alias LoginData = { id: String }
type alias Model = { username : String
                   , password : String
                   , response : Maybe (Result Http.Error LoginData)
                   , mdl : Material.Model
                   }


initialState : Model
initialState = { username = ""
               , password = ""
               , response = Nothing
               , mdl = Material.model
               }

init : ( Model, Cmd Msg )
init = ( initialState, Cmd.none )


{-| MESSAGES

-}

type Msg
    = NoOp
    | ChangeUsername String
    | ChangePassword String
    | Login String String
    | LoginResult (Result Http.Error LoginData)
    | Mdl (Material.Msg Msg)


{-| VIEW

-}

displayResponse : Maybe (Result Http.Error LoginData) -> String
displayResponse res = case res of
                          Just (Ok result) -> result.id
                          Just (Err error) -> toString error
                          Nothing -> "Not Logged in."


type alias Index = List Int
type alias Mdl = Material.Model

simpleInput
    : String
    -> Index
    -> List (Textfield.Property Msg)
    -> String
    -> Mdl
    -> (String -> Msg)
    -> Html Msg
simpleInput label index opts value mdl message =
    let options = (Options.onInput message) :: (opts ++ (inputDefs value label)) in
    div [] [ Textfield.render Mdl index  mdl options [] ]

simpleButton : String -> Index -> Mdl -> Msg -> Html Msg
simpleButton content index mdl msg =
    let options = (Options.onClick msg) :: buttonDefs in
    div [] [ Button.render Mdl index mdl options [ text content] ]


loginForm : Model -> Html Msg
loginForm model =
        form
        [ style [ ("padding", "2rem") ]
        , onSubmit (Login model.username model.password)
        ]
        [ simpleInput "Username" [0] [] model.username model.mdl ChangeUsername
        , simpleInput "Password" [1] [ Textfield.password ] model.password model.mdl ChangePassword
        , simpleButton "Login" [2] model.mdl <| Login model.username model.password
        , Html.p [] [ text (displayResponse model.response) ]
        ]


view : Model -> Html Msg
view model =
    Scheme.topWithScheme Color.Indigo Color.LightBlue
    <| loginForm model

{-| UPDATE

-}

doLogin : String -> String -> Cmd Msg
doLogin uname pw =
    let
        url = userServiceUrl ++ "login"
        body = Encode.object [ ("username", Encode.string uname)
                             , ("password", Encode.string pw) ]
        request = Http.post url (Http.jsonBody body) decodeLogin
    in
        Http.send LoginResult request

decodeLogin : Decode.Decoder LoginData
decodeLogin = Decode.map LoginData (Decode.field "id" Decode.string)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUsername message -> ( { model | username = message }, Cmd.none )
        ChangePassword message -> ( { model | password = message }, Cmd.none )
        Login username password -> ( model, doLogin username password)
        LoginResult result -> ( { model | response = Just result }, Cmd.none )
        Mdl msg_ -> Material.update Mdl msg_ model
        NoOp -> ( model, Cmd.none )


{-| SUBSCRIPTIONS

-}


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


{-| MAIN

-}

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
