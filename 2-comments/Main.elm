module Main exposing (..)

import Avatar
import Date
import Html exposing (br, button, div, form, h1, h3, input, label, li, strong, ul, text, textarea)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (field, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Json.Encode
import String


--
-- Model
--


type alias Comment =
    { author : String
    , content : String
    , email : Avatar.Model
    , date : String
    , saved : Bool
    }


type alias Model =
    { comments : List Comment
    , form : Comment
    , loaded : Bool
    }


initialModel : Model
initialModel =
    Model [] (Comment "" "" Nothing "" False) False



--
-- Update
--


type Msg
    = PostComment
    | UpdateAuthor String
    | UpdateContent String
    | ResetForm
    | CommentsLoaded (Result Http.Error (List Comment))
    | CommentSaved (Result Http.Error Comment)
    | AvatarMsg Avatar.Msg
    | UpdateEmail String


resetForm : Model -> Model
resetForm model =
    { model | form = Comment "" "" Nothing "" False }


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
        CommentsLoaded (Ok comments) ->
            let
                newModel =
                    { model | comments = comments, loaded = True }
            in
                ( newModel, Cmd.none )

        CommentsLoaded (Err error) ->
            let
                err =
                    Debug.log "loadComments failed" error
            in
                ( model, Cmd.none )

        CommentSaved (Ok comment) ->
            let
                newComments =
                    updateCommentsStatus comment model.comments
            in
                ( { model | comments = newComments }, Cmd.none )

        CommentSaved (Err error) ->
            let
                err =
                    Debug.log "saveComment failed" error
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

        UpdateAuthor author ->
            let
                form =
                    model.form

                newForm =
                    { form | author = author }
            in
                ( { model | form = newForm }, Cmd.none )

        UpdateContent content ->
            let
                form =
                    model.form

                newForm =
                    { form | content = content }
            in
                ( { model | form = newForm }, Cmd.none )

        UpdateEmail email ->
            let
                form =
                    model.form

                newForm =
                    { form | email = Just email }
            in
                ( { model | form = newForm }, Cmd.none )

        AvatarMsg msg ->
            ( model, Cmd.none )


commentsUrl : String
commentsUrl =
    "https://vamosaprenderelm.herokuapp.com/api/comments/"


loadComments : Cmd Msg
loadComments =
    Http.send
        CommentsLoaded
        (Http.get commentsUrl decodeComments)


saveComment : Comment -> Cmd Msg
saveComment comment =
    let
        basicData =
            [ ( "author", Json.Encode.string comment.author )
            , ( "content", Json.Encode.string comment.content )
            ]

        email =
            case comment.email of
                Just email ->
                    ( "email", Json.Encode.string email )

                Nothing ->
                    ( "email", Json.Encode.null )

        data =
            email :: basicData |> Json.Encode.object |> Http.jsonBody
    in
        Http.send
            CommentSaved
            (Http.post commentsUrl data decodeComment)



--
-- Decoder
--


decodeComment : Json.Decode.Decoder Comment
decodeComment =
    decode Comment
        |> required "author" string
        |> required "content" string
        |> required "email" (nullable string)
        |> required "date" string
        |> hardcoded True


decodeComments : Json.Decode.Decoder (List Comment)
decodeComments =
    field "comments" (list decodeComment)



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

        avatar =
            Html.map
                (\msg -> AvatarMsg msg)
                (Avatar.view comment.email)

        date =
            Date.fromString comment.date

        dateAsString =
            case date of
                Ok d ->
                    String.concat
                        [ (toString <| Date.day d)
                        , "/"
                        , (toString <| Date.month d)
                        , "/"
                        , (toString <| Date.year d)
                        ]

                Err _ ->
                    ""
    in
        li
            [ class commentClass ]
            [ avatar
            , br [] []
            , strong [] [ text comment.author ]
            , text dateAsString
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
                        , label [] [ text "E-mail" ]
                        , input
                            [ class "form-control"
                            , onInput UpdateEmail
                            , value <| Maybe.withDefault "" model.form.email
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


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, loadComments )
        , update = update
        , subscriptions = (\n -> Sub.none)
        , view = view
        }
