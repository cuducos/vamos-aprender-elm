module Main exposing (..)

import Html exposing (br, button, div, form, h1, h3, input, label, li, strong, ul, text, textarea)
import Html.App
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Http
import Json.Decode exposing ((:=), list, string)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Json.Encode
import String
import Task


--
-- Model
--


type alias Comment =
    { author : String
    , content : String
    , saved : Bool
    }


type alias Model =
    { comments : List Comment
    , form : Comment
    , loaded : Bool
    }


initialModel : Model
initialModel =
    Model [] (Comment "" "" False) False



--
-- Update
--


type Msg
    = PostComment
    | UpdateAuthor String
    | UpdateContent String
    | ApiSuccess (List Comment)
    | ApiFail Http.Error
    | ResetForm
    | CommentSaved Comment


resetForm : Model -> Model
resetForm model =
    { model | form = Comment "" "" False }


updateCommentStatus : Comment -> Comment -> Comment
updateCommentStatus new current =
    if new.author == current.author && new.content == current.content then
        { current | saved = True }
    else
        current


updateCommentsStatus : Comment -> List Comment -> List Comment
updateCommentsStatus newComment comments =
    List.map (updateCommentStatus newComment) comments


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiSuccess comments ->
            let
                model =
                    { model | comments = comments, loaded = True }
            in
                ( model, Cmd.none )

        ApiFail error ->
            let
                err =
                    Debug.log "ApiFail" error
            in
                ( model, Cmd.none )

        ResetForm ->
            ( resetForm model, Cmd.none )

        PostComment ->
            let
                author =
                    model.form.author |> String.trim

                content =
                    model.form.content |> String.trim

                newModel =
                    if (String.isEmpty author) && (String.isEmpty content) then
                        model
                    else
                        { model | comments = List.append model.comments [ model.form ] }
            in
                ( resetForm newModel, saveComment model.form )

        UpdateAuthor value ->
            ( { model | form = Comment value model.form.content False }, Cmd.none )

        UpdateContent value ->
            ( { model | form = Comment model.form.author value False }, Cmd.none )

        CommentSaved comment ->
            let
                newComments =
                    updateCommentsStatus comment model.comments
            in
                ( { model | comments = newComments }, Cmd.none )


commentsUrl : String
commentsUrl =
    "https://vamosaprenderelm.herokuapp.com/api/comments/"


loadComments : Cmd Msg
loadComments =
    Task.perform
        ApiFail
        ApiSuccess
        (Http.get decodeComments commentsUrl)


saveComment : Comment -> Cmd Msg
saveComment comment =
    let
        json =
            Json.Encode.object
                [ ( "author", Json.Encode.string comment.author )
                , ( "content", Json.Encode.string comment.content )
                ]

        data =
            Json.Encode.encode 0 json |> Http.string
    in
        Task.perform
            ApiFail
            CommentSaved
            (Http.post decodeComment commentsUrl data)



--
-- Decoder
--


decodeComment : Json.Decode.Decoder Comment
decodeComment =
    decode Comment
        |> required "author" string
        |> required "content" string
        |> hardcoded True


decodeComments : Json.Decode.Decoder (List Comment)
decodeComments =
    ("comments" := list decodeComment)



--
-- View
--


pluralize : String -> Int -> String
pluralize name count =
    if count == 1 then
        name
    else
        name ++ "s"


viewComment : Comment -> Html.Html Msg
viewComment comment =
    let
        commentClass =
            if comment.saved then
                ""
            else
                "saving"
    in
        li
            [ class commentClass ]
            [ strong [] [ text comment.author ]
            , br [] []
            , text comment.content
            ]


view : Model -> Html.Html Msg
view model =
    if model.loaded then
        let
            count =
                List.length model.comments

            title =
                (toString count) ++ (pluralize " Comentário" count)
        in
            div
                []
                [ h3 [] [ text title ]
                , ul [ class "list-unstyled" ] (List.map viewComment model.comments)
                , form
                    [ onSubmit PostComment ]
                    [ div
                        [ class "form-group" ]
                        [ label [] [ text "Nome:" ]
                        , input
                            [ class "form-control"
                            , onInput UpdateAuthor
                            , value model.form.author
                            ]
                            []
                        ]
                    , div
                        [ class "form-group" ]
                        [ label [] [ text "Comentário:" ]
                        , textarea
                            [ class "form-control"
                            , onInput UpdateContent
                            , value model.form.content
                            ]
                            []
                        ]
                    , button [ onClick ResetForm, class "btn btn-default" ] [ text "Cancelar" ]
                    , button [ class "btn btn-primary" ] [ text "Enviar" ]
                    ]
                ]
    else
        h1 [] [ text "Loading…" ]



--
-- Init
--


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, loadComments )
        , update = update
        , subscriptions = (\n -> Sub.none)
        , view = view
        }
