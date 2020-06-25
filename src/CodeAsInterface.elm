module CodeAsInterface exposing (main)

import Base64
import Browser exposing (Document)
import Browser.Navigation
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import CustomToVirtualDom exposing (toVirtualDom)
import Flate
import Html
import Html.Attributes exposing (attribute)
import Html.Events exposing (onInput)
import Html.Parser as HP
import List
import Task
import Time exposing (Posix)
import Url exposing (Url)


main : Platform.Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                case model.unsavedAt of
                    Nothing ->
                        Sub.none

                    Just _ ->
                        Time.every 200 SaveTextArea
        , onUrlRequest = UrlRequestMsg
        , onUrlChange = \_ -> Noop
        }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init =
    \_ ->
        \url ->
            \key ->
                let
                    decodedFragment =
                        url.fragment |> Maybe.andThen decodeFragment

                    defaultTextarea =
                        """<h1>Code As Interface</h1>
<textarea id="self" style="width: 90%; height: 10em"></textarea>
<div>Try editing the html in the textarea.</div>
<div><a href="https://github.com/snoble/codeasinterface">Source</a>.</div>
"""

                    textarea =
                        decodedFragment |> Maybe.map Tuple.first |> Maybe.withDefault defaultTextarea

                    docText =
                        decodedFragment |> Maybe.map Tuple.second |> Maybe.withDefault defaultTextarea

                    document =
                        textToDocument (Doc "" (Document "" [])) docText textarea
                in
                ( Model textarea document url key Nothing, Cmd.none )


type Msg
    = TextAreaChange String
    | Noop
    | SaveTextArea Posix
    | SaveUnsaved Posix
    | UrlRequestMsg Browser.UrlRequest


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


type alias Doc =
    { text : String
    , document : Document Msg
    }


type alias Model =
    { textarea : String
    , document : Doc
    , url : Url.Url
    , urlKey : Browser.Navigation.Key
    , unsavedAt : Maybe Posix
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


textToDocument : Doc -> String -> String -> Doc
textToDocument alt docText text =
    case HP.run docText of
        Result.Err _ ->
            alt

        Result.Ok lst ->
            let
                body =
                    lst |> extendAttributes (addSpecialAttrToSelf text) |> toVirtualDom toAttribute
            in
            Doc docText (Document "" body)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange change ->
            let
                textarea =
                    change

                document =
                    textToDocument model.document textarea textarea
            in
            ( { model | textarea = textarea, document = document }, Task.perform SaveUnsaved Time.now )

        Noop ->
            ( model, Cmd.none )

        UrlRequestMsg urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Browser.Navigation.load href )

                Browser.Internal url ->
                    ( model, Browser.Navigation.load (Url.toString url) )

        SaveUnsaved t ->
            ( { model | unsavedAt = Just (model.unsavedAt |> Maybe.withDefault t) }, Cmd.none )

        SaveTextArea now ->
            case model.unsavedAt of
                Nothing ->
                    ( model, Cmd.none )

                Just t ->
                    if Time.posixToMillis now - Time.posixToMillis t < 500 then
                        ( model, Cmd.none )

                    else
                        let
                            origUrl =
                                model.url

                            textarea =
                                model.textarea

                            docText =
                                model.document.text

                            newUrl =
                                { origUrl | fragment = encodeStrings [ textarea, docText ] [] }
                        in
                        ( { model | url = newUrl, unsavedAt = Nothing }, Browser.Navigation.pushUrl model.urlKey (Url.toString newUrl) )


decodeString : Decode.Decoder String
decodeString =
    Decode.unsignedInt32 BE
        |> Decode.andThen Decode.string


decodeFragment : String -> Maybe ( String, String )
decodeFragment str =
    let
        decoder =
            Decode.map2 (\s2 -> \s1 -> ( s1, s2 )) decodeString decodeString
    in
    str |> Base64.toBytes |> Maybe.andThen Flate.inflate |> Maybe.andThen (Decode.decode decoder)


encodeStrings : List String -> List Encode.Encoder -> Maybe String
encodeStrings strings encoders =
    case strings of
        [] ->
            Encode.sequence encoders |> Encode.encode |> Flate.deflate |> Base64.fromBytes

        str :: rest ->
            let
                n =
                    str |> Encode.getStringWidth
            in
            encodeStrings rest (encoders |> List.append [ Encode.signedInt32 BE n, Encode.string str ])


view : Model -> Document Msg
view model =
    model.document.document
