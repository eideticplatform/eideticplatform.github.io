port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
import Ports


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
    , modalVisibility : Modal.Visibility
    , radioPhotosPerMonth : RadioPhotosPerMonth
    , radioPaymentMethod : Maybe RadioPaymentMethod
    , email : String
    , reasonablePrice : String
    }


type Page
    = Home
    | ContactUs
    | SubscribePage
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
                , modalVisibility = Modal.hidden
                , radioPhotosPerMonth = Nothing
                , radioPaymentMethod = Nothing
                , email = ""
                , reasonablePrice = ""
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ChangePage Page
    | CloseModal
    | ShowModal
    | AnimateModal Modal.Visibility
    | RadioPhotosMsg RadioPhotosPerMonth
    | RadioPaymentMsg (Maybe RadioPaymentMethod)
    | ConfirmPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg



--subscriptions model =
--    Sub.batch
--        [ Modal.subscriptions model.modalVisibility AnimateModal ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        ChangePage state ->
            ( { model | page = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.batch [ Ports.modalClose () ] )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }, Cmd.batch [ Ports.modalOpen () ] )

        -- Add handling of the animation related messages
        AnimateModal visibility ->
            ( { model | modalVisibility = visibility }, Cmd.none )

        RadioPhotosMsg state ->
            ( { model | radioPhotosPerMonth = state }
            , Cmd.none
            )

        RadioPaymentMsg state ->
            ( { model | radioPaymentMethod = state }
            , Cmd.none
            )

        ConfirmPressed ->
            ( model
            , Debug.log (toString model) (Cmd.none)
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
        , UrlParser.map SubscribePage (UrlParser.s "subscribe")
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
            , Navbar.itemLink [ href "#subscribe" ] [ text "Subscribe" ]
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

            SubscribePage ->
                pageSubscribe model

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
            [ text "Your Favorite Moments At Your Doorstep"
            ]
        , Button.button
            [ Button.outlinePrimary
            , Button.small
            , Button.attrs [ id "subscribe", onClick <| ChangePage SubscribePage ]
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


pageContactUs : Model -> List (Html Msg)
pageContactUs model =
    [ h2 [] [ text "Contact us" ]
    ]


pageSubscribe : Model -> List (Html Msg)
pageSubscribe model =
    [ main_
        [ id "content", style [ ( "padding", "25" ) ] ]
        [ h2 [] [ text "Subscribe" ]
        , Form.form []
            [ Form.group []
                [ Form.label [ for "email" ] [ text "Email address" ]
                , InputGroup.config (InputGroup.email [ Input.id "email", Input.attrs [ value model.email ] ])
                    |> InputGroup.predecessors [ InputGroup.span [] [ text "@" ] ]
                    |> InputGroup.view
                , Form.help [] [ text "Your email will never be shared with anyone else" ]
                ]
            , Form.group []
                [ Form.label [ for "photos" ] [ text "Preferred number of photos per month:" ]
                ]
            , Form.group []
                [ radioPhotosView [ ButtonGroup.attrs [ id "photos" ] ] model
                ]
            , Form.group []
                [ Form.label [ for "price" ] [ text "What do you think is a reasonable price?" ]
                , InputGroup.config (InputGroup.number [ Input.id "price", Input.attrs [ value model.reasonablePrice ] ])
                    |> InputGroup.predecessors [ InputGroup.span [] [ text "$" ] ]
                    |> InputGroup.view
                ]
            , Form.group []
                [ Form.label [ for "payment" ] [ text "Preferred payment method:" ]
                ]
            , Form.group [] [ radioPaymentView [ ButtonGroup.attrs [ id "payment" ] ] model ]
            , Button.button
                [ Button.success
                , Button.attrs [ onClick ConfirmPressed ]
                ]
                [ text "CONFIRM" ]
            ]
        ]
    ]


type alias RadioPhotosPerMonth =
    Maybe Int


photosPerMonthEnum =
    [ 10, 20, 40, 60 ]


type RadioPaymentMethod
    = CashOnDelivery
    | CreditCard
    | AppStore


paymentEnum =
    [ CashOnDelivery, CreditCard, AppStore ]


caption method =
    case method of
        CashOnDelivery ->
            "On Delivery"

        CreditCard ->
            "Credit Card"

        AppStore ->
            "App Store"


radioPhotosView attrs model =
    ButtonGroup.radioButtonGroup attrs
        (List.map
            (\n ->
                ButtonGroup.radioButton
                    (model.radioPhotosPerMonth == (Just n))
                    [ Button.primary, Button.onClick <| RadioPhotosMsg (Just n) ]
                    [ text (toString n) ]
            )
            photosPerMonthEnum
        )


radioPaymentView attrs model =
    ButtonGroup.radioButtonGroup attrs
        (List.map
            (\method ->
                ButtonGroup.radioButton
                    (model.radioPaymentMethod == Just method)
                    [ Button.primary, Button.onClick <| RadioPaymentMsg (Just method) ]
                    [ text (caption method) ]
            )
            paymentEnum
        )


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.withAnimation AnimateModal
        |> Modal.large
        |> Modal.h4 [] [ text "Subscribe" ]
        |> Modal.body []
            [ Form.form []
                [ Form.group []
                    [ Form.label [ for "email" ] [ text "Email address" ]
                    , InputGroup.config (InputGroup.email [ Input.id "email", Input.attrs [ value model.email ] ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "@" ] ]
                        |> InputGroup.view
                    , Form.help [] [ text "Your email will never be shared with anyone else" ]
                    ]
                , Form.group []
                    [ Form.label [ for "photos" ] [ text "Preferred number of photos per month:" ]
                    ]
                , Form.group []
                    [ radioPhotosView [ ButtonGroup.attrs [ id "photos" ] ] model
                    ]
                , Form.group []
                    [ Form.label [ for "price" ] [ text "What do you think is a reasonable price?" ]
                    , InputGroup.config (InputGroup.number [ Input.id "price", Input.attrs [ value model.reasonablePrice ] ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "$" ] ]
                        |> InputGroup.view
                    ]
                , Form.group []
                    [ Form.label [ for "payment" ] [ text "Preferred payment method:" ]
                    ]
                , Form.group [] [ radioPaymentView [ ButtonGroup.attrs [ id "payment" ] ] model ]
                , Button.button
                    [ Button.success
                    , Button.attrs [ onClick ConfirmPressed ]
                    ]
                    [ text "CONFIRM" ]
                ]
            ]
        |> Modal.view model.modalVisibility
