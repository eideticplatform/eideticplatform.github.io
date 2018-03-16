port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, on, onBlur)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
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
import Json.Decode
import Http
import Validate exposing (..)
import Ports
import Convenience exposing (..)
import Set
import Time exposing (millisecond, Time)
import Delay
import Dom.Scroll
import Task
import Dict
import Dom


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
    , radioPhotosPerMonth : Validatable RadioPhotosPerMonth String
    , radioPaymentMethod : Validatable RadioPaymentMethod String
    , email : Validatable String ( Int, String )
    , reasonablePrice : Validatable String ( Int, String )
    , signingUp : Bool
    , signedUp : Bool
    , confirmClicked : Bool
    }


formData : Model -> Maybe (List ( String, String ))
formData model =
    traverse identity
        [ model.email |> validValue
        , model.radioPhotosPerMonth |> Validate.map toString |> validValue
        , model.radioPaymentMethod |> Validate.map caption |> validValue
        , model.reasonablePrice |> validValue
        ]
        |> Maybe.map
            (List.map2 (,) [ "email", "photos", "payment", "price" ])


validateModel : Model -> Model
validateModel model =
    { model
        | reasonablePrice =
            model.reasonablePrice
                |> isNotEmpty ( 0, "Price must not be empty" )
                |> satisfies
                    (\s ->
                        case String.toInt s of
                            Ok _ ->
                                True

                            Err _ ->
                                False
                    )
                    ( 1, "This is not a valid price" )
                |> satisfies
                    (\_ -> validValue model.radioPhotosPerMonth /= Nothing)
                    ( 2, "You must select the number of photos first" )
                |> (let
                        ( mini, maxi ) =
                            model.radioPhotosPerMonth
                                |> validValue
                                |> Maybe.andThen (\k -> Dict.get k photosPriceRanges)
                                |> Maybe.withDefault ( 0, 0 )
                    in
                        satisfies
                            (\n ->
                                case String.toInt n of
                                    Ok n ->
                                        n >= mini && n <= maxi

                                    Err _ ->
                                        False
                            )
                            ( 3, "Price must be between " ++ toString mini ++ " and " ++ toString maxi )
                   )
        , email =
            model.email
                |> isNotEmpty ( 1, "Email must not be empty" )
                |> isEmail ( 2, "This is not a valid email address" )
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
                , radioPhotosPerMonth = empty
                , radioPaymentMethod = empty
                , email = Validate.unchecked ""
                , reasonablePrice = Validate.unchecked ""
                , signingUp = False
                , signedUp = False
                , confirmClicked = False
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type DefocusedField
    = Email
    | Price


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ChangePage Page
    | RadioPhotosMsg RadioPhotosPerMonth
    | RadioPaymentMsg RadioPaymentMethod
    | ConfirmPressed
    | SignupPressed
    | ChangeEmail String
    | ChangePrice String
    | Defocused DefocusedField
    | ValidateModel
    | Response (Result Http.Error String)
    | ScrollTo String
    | EmptyMsg (Result Dom.Error ())
    | Delay ( Float, Msg )


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


message x =
    Task.perform identity (Task.succeed x)


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
            ( let
                newModel =
                    { model
                        | radioPhotosPerMonth = Validate.valid state
                        , reasonablePrice = uncheck model.reasonablePrice
                    }
              in
                if errorIndexOfReasonablePrice model >= 2 then
                    { newModel | reasonablePrice = (validateModel newModel).reasonablePrice }
                else
                    { model | radioPhotosPerMonth = Validate.valid state }
            , Cmd.none
            )

        RadioPaymentMsg state ->
            ( { model | radioPaymentMethod = Validate.valid state }
            , Cmd.none
            )

        SignupPressed ->
            ( { model | signingUp = True }
            , message <| ScrollTo "motto"
            )

        ConfirmPressed ->
            let
                validated =
                    validateModel model

                validatedFormData =
                    formData validated

                valid =
                    validatedFormData /= Nothing
            in
                ( if valid then
                    { validated
                        | signingUp = False
                        , signedUp = True
                        , radioPhotosPerMonth = empty
                        , radioPaymentMethod = empty
                        , email = Validate.unchecked ""
                        , reasonablePrice = Validate.unchecked ""
                        , confirmClicked = False
                    }
                  else
                    { validated | confirmClicked = True }
                , case validatedFormData of
                    Just d ->
                        Cmd.batch
                            [ Ports.sendData (formDatafication d)
                            , message <| ScrollTo "motto"
                            ]

                    Nothing ->
                        message <| ScrollTo "motto"
                )

        ChangeEmail state ->
            ( { model | email = Validate.unchecked state }
            , Cmd.none
            )

        ChangePrice state ->
            ( { model | reasonablePrice = Validate.unchecked state }
            , Cmd.none
            )

        Response _ ->
            ( model, Cmd.none )

        Delay ( wait, msg ) ->
            model ! [ Delay.after wait millisecond msg ]

        Defocused Email ->
            ( { model | email = (validateModel model).email }
            , Cmd.none
            )

        Defocused Price ->
            ( let
                validated =
                    validateModel model
              in
                { model
                    | reasonablePrice = validated.reasonablePrice
                    , radioPhotosPerMonth =
                        if errorIndexOfReasonablePrice validated == 2 then
                            addErrors (Set.singleton "bad") (unchecked -1000)
                        else
                            model.radioPhotosPerMonth
                }
            , Cmd.none
            )

        ValidateModel ->
            ( validateModel model, Cmd.none )

        ScrollTo s ->
            ( model, Ports.scrollTo s )

        EmptyMsg x ->
            ( model, Cmd.none )


errorIndexOfReasonablePrice model =
    model.reasonablePrice
        |> errors
        |> Maybe.withDefault Set.empty
        |> Set.toList
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault (-1)


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
            , Navbar.itemLink [ href "#sign-up" ] [ text "Sign up" ]
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


animation =
    class "animated fadeIn"


pageHome : Model -> List (Html Msg)
pageHome model =
    [ main_
        [ class "bd-masthead", id "content", animation, style [ ( "animation-delay", "400ms" ) ] ]
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
        , p [ id "motto", class "version", style [ ( "margin-bottom", "2rem" ) ] ]
            [ text "Your Favorite Memories At Your Doorstep"
            ]
        , if model.signingUp then
            pageSignup model
          else if model.signedUp then
            pageThanks model
          else
            Button.button
                [ Button.outlinePrimary
                , Button.small
                , Button.attrs [ class "animated wobble", style [ ( "animation-delay", "1s" ) ], id "signup", onClick <| SignupPressed ]
                ]
                [ text "SIGN UP" ]
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


emptyFeedback =
    [ Form.invalidFeedback [ style [ ( "color", "rgba(0,0,0,0)" ) ] ] [ text "hidden" ] ]


invalidFeedback field =
    Maybe.withDefault emptyFeedback
        (Maybe.map
            (\invalidBecause ->
                [ Form.invalidFeedback [ entrance ] [ text (Set.toList invalidBecause |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault "") ] ]
            )
            (Validate.errors field)
        )


emailView : Model -> List (Html Msg)
emailView model =
    [ Form.label [ for "email" ] [ text "Email address" ]
    , InputGroup.config
        (InputGroup.email
            ((case Validate.errors model.email of
                Just _ ->
                    [ Input.danger ]

                Nothing ->
                    []
             )
                ++ [ Input.id "email"
                   , Input.attrs [ autofocus True, onInput ChangeEmail, onBlur (Defocused Email) ]
                   ]
            )
        )
        |> InputGroup.predecessors [ InputGroup.span [] [ text "@" ] ]
        |> InputGroup.view
    ]
        ++ invalidFeedback model.email
        ++ [ Form.help
                ((case Validate.errors model.email of
                    Just _ ->
                        [ class "isDown" ]

                    Nothing ->
                        [ class "isUp" ]
                 )
                )
                [ text "Your email will never be shared with anyone else" ]
           ]


photosView : Model -> List (Html Msg)
photosView model =
    let
        invalid =
            model.confirmClicked && model.radioPhotosPerMonth == empty || errors model.radioPhotosPerMonth /= Nothing
    in
        [ Form.label [ for "photos" ] [ text "Preferred number of photos per month" ]
        , radioPhotosView [ ButtonGroup.attrs [ id "photos" ] ] model invalid
        ]
            ++ (if invalid then
                    [ Form.invalidFeedback [ entrance ] [ text "Please select one of the options above" ] ]
                else
                    emptyFeedback
               )


priceView : Model -> List (Html Msg)
priceView model =
    [ Form.label [ for "price" ] [ text "What do you think is a reasonable price?" ]
    , InputGroup.config
        (InputGroup.number
            ((case Validate.errors model.reasonablePrice of
                Just _ ->
                    [ Input.danger ]

                Nothing ->
                    []
             )
                ++ [ Input.id "price"
                   , Input.attrs
                        ([ onBlur (Defocused Price), onInput ChangePrice ]
                            ++ (Dict.get
                                    (model.radioPhotosPerMonth
                                        |> validValue
                                        |> Maybe.withDefault (-1)
                                    )
                                    photosPriceRanges
                                    |> Maybe.map (\( mi, ma ) -> [ Html.Attributes.min (toString mi), Html.Attributes.max (toString ma) ])
                                    |> Maybe.withDefault []
                               )
                        )
                   ]
            )
        )
        |> InputGroup.predecessors [ InputGroup.span [] [ text "$" ] ]
        |> InputGroup.view
    ]
        ++ invalidFeedback model.reasonablePrice


paymentView : Model -> List (Html Msg)
paymentView model =
    let
        invalid =
            model.confirmClicked && model.radioPaymentMethod == empty
    in
        [ Form.label [ for "payment" ] [ text "Preferred payment method" ]
        , radioPaymentView [ ButtonGroup.attrs [ id "payment" ] ] model invalid
        ]
            ++ (if invalid then
                    [ Form.invalidFeedback [ entrance ] [ text "Please select one of the options" ] ]
                else
                    emptyFeedback
               )


entrance =
    class "fadeInDown animated"


pageSignup : Model -> Html Msg
pageSignup model =
    main_
        [ id "signup_content"
        , style [ ( "padding", "1.2rem" ) ]
        , class "zoomIn animated"
        ]
        [ h2 [ entrance, style [ ( "text-align", "center" ) ] ] [ text "SIGN UP" ]
        , Form.form [] <|
            List.map (Form.group [])
                [ emailView model, photosView model, priceView model, paymentView model ]
                ++ [ Form.label [] [ text "* By signing up before launch, we will send you a free month package with eidetic and service updates." ]
                   , Button.button
                        [ if validateModel model |> formData |> isNothing then
                            Button.danger
                          else
                            Button.success
                        , Button.attrs
                            ([ onClick ConfirmPressed
                             , id "confirm"
                             , type_ "button"
                             , style
                                ([ ( "margin-top", "1rem" ) ]
                                    ++ if isNothing (formData (validateModel model)) then
                                        [ ( "background-color", alertColor )
                                        , ( "border-color", "rgba(237, 74, 60, 0.74)" )
                                        , ( "color", alertForeGround )
                                        ]
                                       else
                                        []
                                )
                             ]
                            )
                        ]
                        [ text "CONFIRM" ]
                   ]
        ]


pageThanks : Model -> Html Msg
pageThanks model =
    main_
        [ id "signup_content", style [ ( "padding", "1.2rem" ) ] ]
        [ h2 [ style [ ( "text-align", "center" ) ] ] [ text "Thanks!" ]
        , h4 [] [ text "Thank you for filling the survey and signing up to eidetic! We will send you an email confirming your free month at launch." ]
        ]


type alias RadioPhotosPerMonth =
    Int


photosPriceRanges =
    Dict.fromList [ ( 10, ( 3, 20 ) ), ( 20, ( 5, 30 ) ), ( 40, ( 7, 40 ) ), ( 60, ( 10, 60 ) ) ]


photosPerMonthEnum =
    photosPriceRanges |> Dict.toList |> List.map Tuple.first


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


alertColor =
    "rgba(230, 70, 64, 0.54)"


alertForeGround =
    "rgba(255, 255, 255, 0.63)"


radioPhotosView : List (ButtonGroup.Option Msg) -> Model -> Bool -> Html Msg
radioPhotosView attrs model invalid =
    ButtonGroup.radioButtonGroup attrs
        (List.map
            (\n ->
                ButtonGroup.radioButton
                    (Validate.validValue model.radioPhotosPerMonth == Just n)
                    [ Button.primary
                    , Button.onClick <| RadioPhotosMsg n
                    , Button.attrs
                        (if invalid then
                            [ style [ ( "background-color", alertColor ), ( "color", alertForeGround ) ] ]
                         else
                            []
                        )
                    ]
                    [ text (toString n) ]
            )
            photosPerMonthEnum
        )


radioPaymentView : List (ButtonGroup.Option Msg) -> Model -> Bool -> Html Msg
radioPaymentView attrs model invalid =
    ButtonGroup.radioButtonGroup attrs
        (List.map
            (\method ->
                ButtonGroup.radioButton
                    (Validate.validValue model.radioPaymentMethod == Just method)
                    [ Button.primary
                    , Button.onClick <| RadioPaymentMsg method
                    , Button.attrs
                        (if invalid then
                            [ style [ ( "background-color", alertColor ), ( "color", alertForeGround ) ] ]
                         else
                            []
                        )
                    ]
                    [ text (caption method) ]
            )
            paymentEnum
        )
