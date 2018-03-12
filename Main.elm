port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, on)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
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
import Ports
import Json.Decode
import Http


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
    , radioPhotosPerMonth : RadioPhotosPerMonth
    , radioPaymentMethod : Maybe RadioPaymentMethod
    , email : String
    , reasonablePrice : String
    , subscribing : Bool
    }


formData : Model -> List ( String, String )
formData model =
    [ ( "email", model.email )
    , ( "photos", toString <| Maybe.withDefault 0 model.radioPhotosPerMonth )
    , ( "payment", Maybe.withDefault "" <| Maybe.map caption model.radioPaymentMethod )
    , ( "price", model.reasonablePrice )
    ]


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
                , radioPhotosPerMonth = Nothing
                , radioPaymentMethod = Nothing
                , email = ""
                , reasonablePrice = ""
                , subscribing = False
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ChangePage Page
    | RadioPhotosMsg RadioPhotosPerMonth
    | RadioPaymentMsg (Maybe RadioPaymentMethod)
    | ConfirmPressed
    | SubscribePressed
    | ChangeEmail String
    | ChangePrice String
    | Response (Result Http.Error String)


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

        RadioPhotosMsg state ->
            ( { model | radioPhotosPerMonth = state }
            , Cmd.none
            )

        RadioPaymentMsg state ->
            ( { model | radioPaymentMethod = state }
            , Cmd.none
            )

        SubscribePressed ->
            ( { model | subscribing = True }
            , Cmd.none
            )

        ConfirmPressed ->
            ( { model | subscribing = False }
            , Ports.sendData (formDatafication (formData model))
            )

        ChangeEmail state ->
            ( { model | email = state }
            , Cmd.none
            )

        ChangePrice state ->
            ( { model | reasonablePrice = state }
            , Cmd.none
            )

        Response _ ->
            ( model, Cmd.none )


formDatafication : List ( String, String ) -> String
formDatafication formData =
    formData
        |> List.map (\( key, value ) -> (Http.encodeUri key) ++ "=" ++ (Http.encodeUri value))
        |> String.join "&"


formDataBody : List ( String, String ) -> Http.Body
formDataBody formData =
    formDatafication formData
        |> Http.stringBody "Content-Type', 'application/x-www-form-urlencoded"


sendData data =
    let
        scriptUrl =
            "https://script.google.com/macros/s/AKfycbxQQN9hZcsWQiHQdfmIIGuWDcrqFiFwK8PAxoEv8I5WO4O7ESCV/exec"

        req =
            Http.request
                { method = "POST"
                , headers = []
                , url = scriptUrl
                , body = formDataBody data
                , expect = Http.expectJson Json.Decode.string
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Response req


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
        , p [ class "version", style [ ( "margin-bottom", "2rem" ) ] ]
            [ text "Your Favorite Memories At Your Doorstep"
            ]
        , if model.subscribing then
            pageSubscribe model
          else
            Button.button
                [ Button.outlinePrimary
                , Button.small
                , Button.attrs [ id "subscribe", onClick <| SubscribePressed ]
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


pageSubscribe : Model -> Html Msg
pageSubscribe model =
    main_
        [ id "subscribe_content", style [ ( "padding", "1.2rem" ) ] ]
        [ h2 [ style [ ( "text-align", "center" ) ] ] [ text "SUBSCRIBE" ]
        , Form.form [ name "submit-to-google-sheet" ]
            [ Form.group []
                [ Form.label [ for "email" ] [ text "Email address" ]
                , InputGroup.config
                    (InputGroup.email
                        [ Input.danger
                        , Input.id "email"
                        , Input.attrs [ name "email", autofocus True, required True, onInput ChangeEmail ]
                        ]
                    )
                    |> InputGroup.predecessors [ InputGroup.span [] [ text "@" ] ]
                    |> InputGroup.view
                , Form.invalidFeedback [] [ text "Something not quite right." ]
                , Form.help [] [ text "Your email will never be shared with anyone else" ]
                ]
            , Form.group []
                [ Form.label [ for "photos" ] [ text "Preferred number of photos per month" ]
                , radioPhotosView [ ButtonGroup.attrs [ id "photos" ] ] model
                , Form.invalidFeedback [] [ text "Something not quite right." ]
                ]
            , Form.group []
                [ Form.label [ for "price" ] [ text "What do you think is a reasonable price?" ]
                , InputGroup.config (InputGroup.number [ Input.id "price", Input.attrs [ name "price", required True, onInput ChangePrice, Html.Attributes.max "20", Html.Attributes.min "3" ] ])
                    |> InputGroup.predecessors [ InputGroup.span [] [ text "$" ] ]
                    |> InputGroup.view
                ]
            , Form.group []
                [ Form.label [ for "payment" ] [ text "Preferred payment method" ]
                , radioPaymentView [ ButtonGroup.attrs [ id "payment" ] ] model
                ]
            , Form.label [] [ text "* By subscribing before launch, we will send you a free month package with eidetic and service updates." ]
            , Button.button
                [ Button.success
                , Button.attrs [ type_ "button", onClick ConfirmPressed, id "confirm", style [ ( "margin-top", "1rem" ) ] ]
                ]
                [ text "CONFIRM" ]
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
                ButtonGroup.myRadio
                    (model.radioPhotosPerMonth == (Just n))
                    [ Button.primary, Button.onClick <| RadioPhotosMsg (Just n) ]
                    [ name "photos", required True ]
                    [ text (toString n) ]
            )
            photosPerMonthEnum
        )


radioPaymentView attrs model =
    ButtonGroup.radioButtonGroup attrs
        (List.map
            (\method ->
                ButtonGroup.myRadio
                    (model.radioPaymentMethod == Just method)
                    [ Button.primary, Button.onClick <| RadioPaymentMsg (Just method) ]
                    [ name "payment", required True ]
                    [ text (caption method) ]
            )
            paymentEnum
        )
