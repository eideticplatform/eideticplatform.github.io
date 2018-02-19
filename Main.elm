module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.CDN as CDN
import Bootstrap.Alert as Alert
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Fieldset as Fieldset
import Markdown


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    , radioPhotosPerMonth : Maybe RadioPhotosPerMonth
    , radioPaymentMethod : Maybe RadioPaymentMethod
    }


type Page
    = Home
    | ContactUs
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location
                { navState = navState
                , page = Home
                , modalState = Modal.hiddenState
                , radioPhotosPerMonth = Nothing
                , radioPaymentMethod = Nothing
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State
    | RadioPhotosMsg (Maybe RadioPhotosPerMonth)
    | RadioPaymentMsg (Maybe RadioPaymentMethod)


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        ModalMsg state ->
            ( { model | modalState = state }
            , Cmd.none
            )

        RadioPhotosMsg state ->
            ( { model | radioPhotosPerMonth = state }
            , Cmd.none
            )

        RadioPaymentMsg state ->
            ( { model | radioPaymentMethod = state }
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map ContactUs (UrlParser.s "contact-us")
        ]


view : Model -> Html Msg
view model =
    div []
        [ mainContent model
        , modal model
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.info
        |> Navbar.brand [ href "#" ] [ text "Eidetic" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#contact-us" ] [ text "Contact us" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            ContactUs ->
                pageContactUs model

            NotFound ->
                pageHome model


pageHome : Model -> List (Html Msg)
pageHome model =
    [ main_
        [ class "bd-masthead", id "content" ]
        [ div
            [ style [ ( "margin", "0 auto 2rem" ) ] ]
            [ img
                [ src "assets/imgs/Logo.svg"
                , alt "eidetic-logo"
                , style
                    [ ( "border", "0.3rem solid white" )
                      --, ( "width", "40%" )
                    , ( "border-radius", "15%" )
                    ]
                , id "logo"
                ]
                []
            ]
        , p [ class "lead" ]
            [ text "EIDETIC" ]
        , p [ class "version" ]
            [ text "Your Favorite Moments"
            ]
        , Button.button
            [ Button.outlinePrimary
            , Button.small
            , Button.attrs [ id "subscribe", onClick <| ModalMsg Modal.visibleState ]
            ]
            [ text "SUBSCRIBE" ]
        , div
            [ style [ ( "margin", "2rem 0 2rem auto" ) ] ]
            [ img
                [ src "assets/imgs/Step1.svg"
                , alt "eidetic-logo"
                , class "step"
                ]
                []
            ]
        , div
            [ style [ ( "margin", "0 auto 2rem" ) ] ]
            [ img
                [ src "assets/imgs/Step2.svg"
                , alt "eidetic-logo"
                , class "step"
                ]
                []
            ]
        , div
            [ style [ ( "margin", "0 auto 2rem" ) ] ]
            [ img
                [ src "assets/imgs/Step3.svg"
                , alt "eidetic-logo"
                , class "step"
                ]
                []
            ]
        ]
    ]


markdown =
    """

# This is Markdown

[Markdown](http://daringfireball.net/projects/markdown/) lets you
write content in a really natural way.

  * You can have lists, like this one
  * Make things **bold** or *italic*
  * Embed snippets of `code`
  * Create [links](/)
  * ...

The [elm-markdown][] package parses all this content, allowing you
to easily generate blocks of `Element` or `Html`.

[elm-markdown]: http://package.elm-lang.org/packages/evancz/elm-markdown/latest

"""



-- inear-gradient(200deg,rgb(218, 57, 4),#801a3e,#410047);


pageContactUs : Model -> List (Html Msg)
pageContactUs model =
    [ h2 [] [ text "Contact us" ]
    ]



--pageModules : Model -> List (Html Msg)
--pageModules model =
--    [ h1 [] [ text "Modules" ]
--    , Alert.warning [ text "Alert" ]
--    , Alert.info [ text "Badge" ]
--    , Alert.success [ text "Card" ]
--    ]
--pageNotFound : List (Html Msg)
--pageNotFound =
--    [ h1 [] [ text "Not found" ]
--    , text "Sorry couldn't find that page"
--    ]


type RadioPhotosPerMonth
    = Ten
    | Twenty
    | Fourty
    | Sixty


type RadioPaymentMethod
    = CashOnDelivery
    | CreditCard
    | AppStore


radioPhotosView attrs model =
    ButtonGroup.radioButtonGroup attrs
        [ ButtonGroup.radioButton
            (model.radioPhotosPerMonth == Just Ten)
            [ Button.primary, Button.onClick <| RadioPhotosMsg (Just Ten) ]
            [ text "10" ]
        , ButtonGroup.radioButton
            (model.radioPhotosPerMonth == Just Twenty)
            [ Button.primary, Button.onClick <| RadioPhotosMsg (Just Twenty) ]
            [ text "20" ]
        , ButtonGroup.radioButton
            (model.radioPhotosPerMonth == Just Fourty)
            [ Button.primary, Button.onClick <| RadioPhotosMsg (Just Fourty) ]
            [ text "40" ]
        , ButtonGroup.radioButton
            (model.radioPhotosPerMonth == Just Sixty)
            [ Button.primary, Button.onClick <| RadioPhotosMsg (Just Sixty) ]
            [ text "60" ]
        ]


radioPaymentView attrs model =
    ButtonGroup.radioButtonGroup attrs
        [ ButtonGroup.radioButton
            (model.radioPaymentMethod == Just CashOnDelivery)
            [ Button.primary, Button.onClick <| RadioPaymentMsg (Just CashOnDelivery) ]
            [ text "Payment on delivery" ]
        , ButtonGroup.radioButton
            (model.radioPaymentMethod == Just CreditCard)
            [ Button.primary, Button.onClick <| RadioPaymentMsg (Just CreditCard) ]
            [ text "Credit card" ]
        , ButtonGroup.radioButton
            (model.radioPaymentMethod == Just AppStore)
            [ Button.primary, Button.onClick <| RadioPaymentMsg (Just AppStore) ]
            [ text "AppStore" ]
        ]


modal : Model -> Html Msg
modal model =
    Modal.config ModalMsg
        |> Modal.large
        |> Modal.h4 [] [ text "Subscribe" ]
        |> Modal.body []
            [ Form.form []
                [ Form.group []
                    [ Form.label [ for "email" ] [ text "Email address" ]
                    , InputGroup.config (InputGroup.email [ Input.id "email" ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "@" ] ]
                        |> InputGroup.view
                    , Form.help [] [ text "Your email will never be shared with anyone else" ]
                    ]
                , Form.group []
                    [ Form.label [ for "question1" ] [ text "Preferred number of photos per month:" ]
                    ]
                , Form.group []
                    [ radioPhotosView [] model
                    ]
                , Form.group []
                    [ Form.label [ for "question2" ] [ text "What do you think is a reasonable price?" ]
                    , InputGroup.config (InputGroup.number [ Input.id "question2" ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "$" ] ]
                        |> InputGroup.view
                    ]
                , Form.group []
                    [ Form.label [ for "question3" ] [ text "Preferred payment method:" ]
                    ]
                , Form.group [] [ radioPaymentView [] model ]
                , Button.button
                    [ Button.success
                    ]
                    [ text "CONFIRM" ]
                ]
            ]
        |> Modal.view model.modalState
