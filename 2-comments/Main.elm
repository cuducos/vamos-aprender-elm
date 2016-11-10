module Main exposing (..)

import Html exposing (br, button, div, form, h1, input, li, strong, ul, text, textarea)
import Html.App
import Html.Attributes exposing (value)
import Http
import Html.Events exposing (onInput, onSubmit)
import Json.Decode exposing ((:=), list, object2, string)
import Task


--
-- Model
--


type alias Comment =
    { author : String
    , content : String
    }


type alias Model =
    { comments : List Comment
    , form : Comment
    }


initialModel : Model
initialModel =
    Model [] (Comment "" "")



--
-- Update
--


type Msg
    = PostComment
    | UpdateAuthor String
    | UpdateContent String
    | ApiSuccess (List Comment)
    | ApiFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiSuccess comments ->
            ( { model | comments = comments }, Cmd.none )

        ApiFail error ->
            let
                err =
                    Debug.log "ApiFail" error
            in
                ( model, Cmd.none )

        PostComment ->
            let
                model =
                    { comments = List.append model.comments [ model.form ]
                    , form = Comment "" ""
                    }
            in
                ( model, Cmd.none )

        UpdateAuthor value ->
            ( { model | form = Comment value model.form.content }, Cmd.none )

        UpdateContent value ->
            ( { model | form = Comment model.form.author value }, Cmd.none )


loadComments : Cmd Msg
loadComments =
    Task.perform
        ApiFail
        ApiSuccess
        (Http.get decode "http://localhost:5000/api/comments/")



--
-- Decoder
--


decodeComment : Json.Decode.Decoder Comment
decodeComment =
    object2 Comment
        ("author" := string)
        ("content" := string)


decode : Json.Decode.Decoder (List Comment)
decode =
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
    li
        []
        [ strong [] [ text comment.author ]
        , br [] []
        , text comment.content
        ]


view : Model -> Html.Html Msg
view model =
    let
        count =
            List.length model.comments

        title =
            (toString count) ++ (pluralize " Comentário" count)
    in
        div
            []
            [ h1 [] [ text title ]
            , ul [] (List.map viewComment model.comments)
            , form
                [ onSubmit PostComment ]
                [ text "Nome:"
                , br [] []
                , input [ onInput UpdateAuthor, value model.form.author ] []
                , br [] []
                , text "Comentário:"
                , br [] []
                , textarea [ onInput UpdateContent, value model.form.content ] []
                , br [] []
                , button [] [ text "Enviar" ]
                ]
            ]



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
