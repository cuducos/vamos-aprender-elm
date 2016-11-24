module Main exposing (..)

import Avatar
import Date
import Html exposing (a, br, button, div, form, h1, h3, input, label, li, strong, ul, text, textarea)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (field, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Json.Encode
import Navigation
import String
import Time


--
-- Model
--


type Page
    = Comments
    | Form


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
    , now : Time.Time
    , page : Page
    }


initialModel : Model
initialModel =
    Model [] (Comment "" "" Nothing "" False) False 0.0 Form



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
    | Tick Time.Time
    | UpdateUrl Navigation.Location
    | MoveTo Page


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

        Tick time ->
            ( { model | now = Time.inSeconds time }, Cmd.none )

        UpdateUrl location ->
            ( urlParser model location, Cmd.none )

        MoveTo page ->
            let
                hash =
                    case page of
                        Form ->
                            "#form"

                        Comments ->
                            "#"
            in
                ( model, Navigation.newUrl hash )


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


dateAgoinMinutes : Float -> String
dateAgoinMinutes seconds =
    let
        ago =
            (seconds / 60)
                |> floor
                |> toString
    in
        String.concat [ " há ", ago, " minutos" ]


viewComment : Time.Time -> Comment -> Html.Html Msg
viewComment now comment =
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

        time =
            case date of
                Ok d ->
                    Date.toTime d
                        |> Time.inSeconds
                        |> (-) now

                Err _ ->
                    0.0

        ago =
            if now > 0.0 then
                dateAgoinMinutes time
            else
                ""
    in
        li
            [ class commentClass ]
            [ avatar
            , br [] []
            , strong [] [ text comment.author ]
            , text ago
            , br [] []
            , text comment.content
            ]


viewComments : Model -> Html.Html Msg
viewComments model =
    if model.loaded then
        let
            count =
                List.length model.comments

            title =
                (toString count) ++ (pluralize " Comentário" count)

            viewCommentNow =
                viewComment model.now
        in
            div
                []
                [ h3 [] [ text title ]
                , ul [ class "list-unstyled" ] (List.map viewCommentNow model.comments)
                ]
    else
        h1 [] [ text "Loading…" ]


viewForm : Model -> Html.Html Msg
viewForm model =
    div
        []
        [ h3 [] [ text "Comente aqui" ]
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


view : Model -> Html.Html Msg
view model =
    let
        main =
            case model.page of
                Comments ->
                    viewComments model

                Form ->
                    viewForm model

        menu =
            div
                []
                [ a [ MoveTo Comments |> onClick ] [ text "Ver comentários" ]
                , text " | "
                , a [ MoveTo Form |> onClick ] [ text "Comentar" ]
                ]
    in
        div [] [ menu, main ]



--
-- Subscription
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick



--
-- Init
--


urlParser : Model -> Navigation.Location -> Model
urlParser model location =
    if location.hash == "#form" then
        { model | page = Form }
    else
        { model | page = Comments }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( urlParser initialModel location, loadComments )


main : Program Never Model Msg
main =
    Navigation.program
        UpdateUrl
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
