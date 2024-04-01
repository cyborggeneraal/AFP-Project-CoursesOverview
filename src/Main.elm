module Main exposing (..)

import Url
import Browser
import Html exposing (Html, div, text, table, thead, tr, th, tbody, td)
import Http
import Json.Decode as Decode exposing (Decoder, field)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html exposing (input)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Html.Attributes exposing (style)
import Html exposing (h1)
import Html.Attributes exposing (checked)

type alias Login =
    { username : String
    }

type alias Course = 
    {   term : String
        , timeSlot : String
        , courseID : String
        , level : String
        , ecName : String
        , capacity : Int
        , checked : Bool
    }

type alias User =
    {
        name : String
        , age : Int
        , email : String        
    }

type alias Model =
    { courses : List Course
    , error : Maybe String
    , content : String
    , prerequisites : List Course
    , login : Maybe Login
    , usernameInput : String
    }

type Msg
    = FetchCourses
    | ReceiveCourses (Result Http.Error (List Course))
    | Change String
    | FetchPrerequisites String
    | ReceivePrerequisites (Result Http.Error (List Course))
    | ReceiveCompletedCourses (Result Http.Error (List Course))
    | FetchCompletedCourses
    | ResetCheckedFields
    | LoginMsg String
    | CheckUsername
    | ReceiveUsername (Result Http.Error User)
    | Logout

init : Model
init = { courses = [], error = Nothing, content = "", prerequisites = [], login = Nothing, usernameInput = ""}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change newContent ->
            ({ model | content = newContent, prerequisites = [] }, fetchCoursesByID newContent)

        FetchCourses ->
            ({model | content = "", prerequisites = []}, fetchCourses)

        ReceiveCourses result ->
            case result of
                Ok courses ->
                    ({ model | courses = courses, error = Nothing }, Cmd.none)

                Err _ ->
                    ({ model | error = Just "An error occurred while fetching courses." }, Cmd.none)
        FetchPrerequisites courseID ->
            (model, fetchPrerequisites courseID)
        ReceivePrerequisites result ->
            case result of
                Ok prerequisites ->
                    ({ model | prerequisites = prerequisites, error = Nothing }, Cmd.none)
                Err _ ->
                    ({ model | error = Just "An error occurred while fetching prerequisites." }, Cmd.none)
        LoginMsg username ->
            ({ model | usernameInput = username }, Cmd.none)
        Logout ->
            ({ model | login = Nothing, courses = [], content = "", prerequisites = []}, Cmd.none)
        CheckUsername ->
            (model, checkUsernameExists model.usernameInput)
        ReceiveUsername result ->
            case result of
                Ok _ ->
                    ({model| login = Just {username = model.usernameInput}}, fetchCourses)
                Err _ ->
                    ({ model | error = Just "Username not found." }, Cmd.none)
        FetchCompletedCourses ->
            (model, fetchCompletedCoursesForUser model.usernameInput) -- TODO: get information from login
        ReceiveCompletedCourses result -> handleCompletedCourses result model
        ResetCheckedFields -> handleResetCheckedFields model

handleCompletedCourses : Result Http.Error (List Course) -> Model -> (Model, Cmd Msg)
handleCompletedCourses result model =
    case result of
        Ok courses ->
            let
                updateCourse course =
                    if List.any (\completedCourse -> completedCourse.courseID == course.courseID) courses then
                        { course | checked = True }
                    else
                        course
            in
            ( { model 
                | courses = List.map updateCourse model.courses
                , prerequisites = List.map updateCourse model.prerequisites
                , error = Nothing 
              }, Cmd.none )

        Err _ ->
            ( { model | error = Just "An error occurred while fetching completed courses." }, Cmd.none )

handleResetCheckedFields : Model -> (Model, Cmd Msg)
handleResetCheckedFields model =
    let
        updateCourse course =
            { course | checked = False }
    in
    ( { model | courses = List.map updateCourse model.courses
                , prerequisites = List.map updateCourse model.prerequisites
                , error = Nothing }, Cmd.none )

-- the use of updateCourse in the last two event handlers could be done with one map function
-- that changes the checked field to the opposite of what it is now in the future.

checkUsernameExists : String -> Cmd Msg
checkUsernameExists username =
    Http.get
        { url = "http://localhost:8081/users/" ++ Url.percentEncode username
        , expect = Http.expectJson ReceiveUsername loginDecoder
        }

fetchPrerequisites : String -> Cmd Msg
fetchPrerequisites search =
    Http.get
        { url = "http://localhost:8081/courses/" ++ Url.percentEncode search ++ "/prereq"
        , expect = Http.expectJson ReceivePrerequisites coursesDecoder
        }

fetchCourses : Cmd Msg
fetchCourses =
    Http.get
        { url = "http://localhost:8081/courses"
        , expect = Http.expectJson ReceiveCourses coursesDecoder
        }

fetchCoursesByID : String -> Cmd Msg
fetchCoursesByID search =
    Http.get
        { url = "http://localhost:8081/courses/" ++ Url.percentEncode search
        , expect = Http.expectJson ReceiveCourses coursesDecoder
        }

fetchCompletedCoursesForUser : String -> Cmd Msg
fetchCompletedCoursesForUser username =
    Http.get
        { url = "http://localhost:8081/users/" ++ Url.percentEncode username ++ "/courses"
        , expect = Http.expectJson ReceiveCompletedCourses coursesDecoder
        }

coursesDecoder : Decoder (List Course)
coursesDecoder =
    Decode.list courseDecoder

courseDecoder : Decoder Course
courseDecoder =
    Decode.map7 Course
        (field "term" Decode.string)
        (field "timeSlot" Decode.string)
        (field "courseID" Decode.string)
        (field "level" Decode.string)
        (field "ecName" Decode.string)
        (field "capacity" Decode.int)
        (Decode.succeed False)

loginDecoder : Decoder User
loginDecoder =
    Decode.map3 User
        (field "name" Decode.string)
        (field "age" Decode.int)
        (field "email" Decode.string)


-- ######### VIEW ##########
view : Model -> Html Msg
view model =
    case model.login of
        Nothing ->
            loginView

        Just _ ->
            mainView model
            
loginView : Html Msg
loginView =
    div []
        [ h1 [] [ text "Please login" ]
        , input [ type_ "text", placeholder "Username", onInput LoginMsg ] []
        , button [ onClick CheckUsername ] [ text "Check Username" ]
        ]

mainView : Model -> Html Msg
mainView model =
    div []
        [ 
        input [ type_ "text", placeholder "Search", value model.content, onInput Change ] []
        , button [ onClick FetchCourses ] [ text "Fetch Courses" ]
        , button [ onClick FetchCompletedCourses ] [ text "Fetch Completed Courses" ]
        , button [ onClick ResetCheckedFields ] [ text "Reset Checked Courses" ]
        , button [ onClick Logout ] [ text "Logout" ]
        , div [ style "display" "flex" ]
            [ div [ style "margin-right" "20px" ] [ coursesTable model.courses ]
            , if List.isEmpty model.prerequisites then
                text ""
              else
                prerequisitesTable model.prerequisites
            ]
        ]

courseRows : List Course -> List (Html Msg)
courseRows courses =
    List.concatMap courseRow courses

courseRow : Course -> List (Html Msg)
courseRow course =
    [ tr [onClick (FetchPrerequisites course.courseID)]
        [ td [] [ text (course.term)]
        , td [] [ text course.timeSlot ]
        , td [] [ text course.courseID ]
        , td [] [ text course.level ]
        , td [] [ text course.ecName ]
        , td [] [ text (String.fromInt course.capacity) ]
        , td [] [ input [ type_ "checkbox", checked course.checked] [] ]
        ]
    ]

prereqRows : List Course -> List (Html Msg)
prereqRows prerequisites =
    List.concatMap prereqRow prerequisites

prereqRow : Course -> List (Html Msg)
prereqRow course =
    [ tr []
        [ td [] [ text (course.term)]
        , td [] [ text course.courseID ]
        , td [] [ text course.level ]
        , td [] [ text course.ecName ]
        , td [] [ input [ type_ "checkbox", checked course.checked] [] ]
        ]
    ]

coursesTable : List Course -> Html Msg
coursesTable courses =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Term" ]
                , th [] [ text "Time Slot" ]
                , th [] [ text "Course ID" ]
                , th [] [ text "Level" ]
                , th [] [ text "Course Name" ]
                , th [] [ text "Capacity" ]
                , th [] [ text "Course completed" ]
                ]
            ]
        , tbody [] (courseRows courses)
        ]

prerequisitesTable : List Course -> Html Msg
prerequisitesTable prerequisites =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Term" ]
                , th [] [ text "Course ID" ]
                , th [] [ text "Level" ]
                , th [] [ text "Course Name" ]
                , th [] [ text "Course Completed" ]
                ]
            ]
        , tbody [] (prereqRows prerequisites)
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }