module Avatar exposing (..)

import Html exposing (img)
import Html.Attributes exposing (src)
import MD5


--
-- Model
--


type alias Model =
    Maybe String


model : Model
model =
    Nothing



--
-- Update
--


type Msg
    = UpdateEmail String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateEmail email ->
            Just email



--
-- View
--


view : Model -> Html.Html Msg
view email =
    case email of
        Just value ->
            let
                hash =
                    MD5.hex value

                url =
                    "https://www.gravatar.com/avatar/" ++ hash ++ "?d=identicon"
            in
                img [ src url ] []

        Nothing ->
            img [ src "https://www.gravatar.com/avatar/?d=mm" ] []
