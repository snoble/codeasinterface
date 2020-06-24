module CodeAsInterface exposing (main)

import Browser exposing (Document)
import CustomToVirtualDom exposing (toVirtualDom)
import Html
import Html.Attributes exposing (attribute)
import Html.Events exposing (onInput)
import Html.Parser as HP
import List


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            textarea =
                "<textarea id=\"self\"></textarea>"

            document =
                textToDocument (Document "" []) textarea
        in
        ( Model textarea document, Cmd.none )


type Msg
    = TextAreaChange String


extendAttributes : (String -> List HP.Attribute -> List HP.Node -> ( List HP.Attribute, List HP.Node )) -> List HP.Node -> List HP.Node
extendAttributes fn =
    List.map
        (\nd ->
            case nd of
                HP.Element tpe attrs children ->
                    let
                        ( more_attrs, more_children ) =
                            fn tpe attrs children

                        result_attrs =
                            attrs |> List.append more_attrs

                        result_children =
                            children |> extendAttributes fn |> List.append more_children
                    in
                    HP.Element tpe result_attrs result_children

                _ ->
                    nd
        )


type alias Model =
    { textarea : String
    , document : Document Msg
    }


addSpecialAttrToSelf : String -> String -> List HP.Attribute -> List HP.Node -> ( List HP.Attribute, List HP.Node )
addSpecialAttrToSelf input tpe attrs _ =
    if tpe == "textarea" && (attrs |> List.member ( "id", "self" )) then
        ( [ ( "@", "@" ) ], [ HP.Text input ] )

    else
        ( [], [] )


toAttribute : HP.Attribute -> Html.Attribute Msg
toAttribute ( name, value ) =
    if name == "@" && value == "@" then
        onInput TextAreaChange

    else
        attribute name value


textToDocument : Document Msg -> String -> Document Msg
textToDocument alt text =
    case HP.run text of
        Result.Err _ ->
            { alt | title = "err" }

        Result.Ok lst ->
            let
                body =
                    lst |> extendAttributes (addSpecialAttrToSelf text) |> toVirtualDom toAttribute
            in
            { body = body, title = "good" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange change ->
            let
                textarea =
                    change

                document =
                    textToDocument model.document textarea
            in
            ( { textarea = textarea, document = document }, Cmd.none )


view : Model -> Document Msg
view model =
    model.document
