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
import Html exposing (h2)

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
    , groupedCourses : List (String, List Course)
    , showGroupedCourses : Bool
    }

type Msg
    = LoginMsg String
    | CheckUsername
    | ReceiveUsername (Result Http.Error User)
    | FetchCourses
    | ReceiveCourses (Result Http.Error (List Course))
    | Change String
    | FetchPrerequisites String
    | ReceivePrerequisites (Result Http.Error (List Course))
    | FetchCompletedCourses
    | ReceiveCompletedCourses (Result Http.Error (List Course))
    | ResetCheckedFields
    | FetchGroupedCourses
    | ReceiveGroupedCourses (Result Http.Error (List (String, List Course)))
    | ShowGroupedCourses
    | Logout

init : Model
init = { courses = []
        , error = Nothing
        , content = ""
        , prerequisites = []
        , login = Nothing
        , usernameInput = ""
        , groupedCourses = []
        , showGroupedCourses = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoginMsg username -> handleLoginMsg username model
        CheckUsername -> handleCheckUsername model
        ReceiveUsername result -> handleReceiveUsername result model
        FetchCourses -> handleFetchCourses model
        ReceiveCourses result -> handleReceiveCourses result model
        Change newContent -> handleChange newContent model
        FetchPrerequisites courseID -> handleFetchPrerequisites courseID model
        ReceivePrerequisites result -> handleReceivePrerequisites result model
        FetchCompletedCourses -> handleFetchCompletedCourses model
        ReceiveCompletedCourses result -> handleCompletedCourses result model
        ResetCheckedFields -> handleResetCheckedFields model
        FetchGroupedCourses -> handleGroupedCourses model
        ReceiveGroupedCourses result -> handleReceivedGroupedCourses result model
        ShowGroupedCourses -> handleShowGroupedCourses model
        Logout -> handleLogout model


-- ######### HANDLERS/UPDATE ##########

handleShowGroupedCourses : Model -> (Model, Cmd Msg)
handleShowGroupedCourses model = ({ model | showGroupedCourses = True
                                            , prerequisites = []
                                            , content = "" }, fetchGroupedCourses)

handleReceivedGroupedCourses :  Result Http.Error (List (String, List Course)) -> Model -> (Model, Cmd Msg)
handleReceivedGroupedCourses result model = 
    case result of
        Ok gc ->
            ({ model | groupedCourses = gc, error = Nothing }, Cmd.none)
        Err _ ->
            ({ model | error = Just "An error occurred while fetching grouped courses." }, Cmd.none)

handleGroupedCourses : Model -> (Model, Cmd Msg)
handleGroupedCourses model =
    (model, fetchGroupedCourses)

handleChange : String -> Model -> (Model, Cmd Msg)
handleChange newContent model =
    ({ model | content = newContent, prerequisites = [] }, fetchCoursesByID newContent)

handleFetchCourses : Model -> (Model, Cmd Msg)
handleFetchCourses model =
    ({model | content = ""
            , prerequisites = []
            , showGroupedCourses = False}, fetchCourses)

handleReceiveCourses : Result Http.Error (List Course) -> Model -> (Model, Cmd Msg)
handleReceiveCourses result model =
    case result of
        Ok courses ->
            ({ model | courses = courses, error = Nothing }, Cmd.none)
        Err _ ->
            ({ model | error = Just "An error occurred while fetching courses." }, Cmd.none)

handleFetchPrerequisites : String -> Model -> (Model, Cmd Msg)
handleFetchPrerequisites courseID model =
    (model, fetchPrerequisites courseID)

handleReceivePrerequisites : Result Http.Error (List Course) -> Model -> (Model, Cmd Msg)
handleReceivePrerequisites result model =
    case result of
        Ok prerequisites ->
            ({ model | prerequisites = prerequisites, error = Nothing }, Cmd.none)
        Err _ ->
            ({ model | error = Just "An error occurred while fetching prerequisites." }, Cmd.none)

handleLoginMsg : String -> Model -> (Model, Cmd Msg)
handleLoginMsg username model =
    ({ model | usernameInput = username }, Cmd.none)

handleLogout : Model -> (Model, Cmd Msg)
handleLogout model =
    ({ model | login = Nothing, courses = [], content = "", prerequisites = []}, Cmd.none)

handleCheckUsername : Model -> (Model, Cmd Msg)
handleCheckUsername model =
    (model, checkUsernameExists model.usernameInput)

handleReceiveUsername : Result Http.Error User -> Model -> (Model, Cmd Msg)
handleReceiveUsername result model =
    case result of
        Ok _ ->
            ({model| login = Just {username = model.usernameInput}}, fetchCourses)
        Err _ ->
            ({ model | error = Just "Username not found." }, Cmd.none)

handleFetchCompletedCourses : Model -> (Model, Cmd Msg)
handleFetchCompletedCourses model =
    (model, fetchCompletedCoursesForUser model.usernameInput)

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

-- ############## END OF HANDLERS/UPDATE ##############

-- ######### HTTP REQUESTS #########
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

fetchGroupedCourses : Cmd Msg
fetchGroupedCourses =
    Http.get
        { url = "http://localhost:8081/tracks-courses"
        , expect = Http.expectJson ReceiveGroupedCourses (Decode.list courseTrackDecoder)
        }
-- ######### END OF HTTP REQUESTS #########

-- ######### DECODERS #########

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

courseTrackDecoder : Decoder (String, List Course)
courseTrackDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.index 1 (Decode.list courseDecoder))

-- ######### END OF DECODERS #########

-- ######### VIEW #########

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
        , button [ onClick ShowGroupedCourses ] [ text "Show Grouped Courses" ]
        , button [ onClick Logout ] [ text "Logout" ]
        , div [ style "display" "flex" ]
            [ div [ style "margin-right" "20px" ]
            -- this is a bit of a hack, but it works for now
            [ if model.showGroupedCourses then viewGroupedCourses model
                                          else coursesTable model.courses ]
            , if List.isEmpty model.prerequisites then
                text ""
              else
                prerequisitesTable model.prerequisites
            ]
        ]

viewGroupedCourses : Model -> Html Msg
viewGroupedCourses model =
    let
        filteredCourses = List.map (\(group, courses) -> (group, List.filter (\course -> String.contains model.content course.courseID) courses)) model.groupedCourses
    in
    div []
        [ div [] (List.map viewGroup filteredCourses) ]

viewGroup : (String, List Course) -> Html Msg
viewGroup (groupName, courses) =
    div []
        [ h2 [] [ text groupName ]
        , table []
            [ thead [] [ tableHeaders ]
            , courseRows courses
            ]
        ]
courseRow : Course -> Html Msg
courseRow course =
    tr [onClick (FetchPrerequisites course.courseID)] ( courseColumns course)

courseRows : List Course -> Html Msg
courseRows courses =
    tbody [] (List.map courseRow courses)
prereqRows : List Course -> Html Msg
prereqRows prerequisites =
    tbody [] (List.map prereqRow prerequisites)

prereqRow : Course -> (Html Msg)
prereqRow course = tr [] (courseColumns course)

coursesTable : List Course -> Html Msg
coursesTable courses =
    table []
        [ thead [] [ tableHeaders ]
        , courseRows courses
        ]

prerequisitesTable : List Course -> Html Msg
prerequisitesTable prerequisites =
    table []
        [ thead [] [ tableHeaders ]
        , prereqRows prerequisites
        ]

tableHeaders : Html Msg
tableHeaders =
    tr []
        [ th [] [ text "Course ID" ]
        , th [] [ text "Course Name" ]
        , th [] [ text "Level" ]
        , th [] [ text "Term" ]
        , th [] [ text "Time Slot" ]
        , th [] [ text "Capacity" ]
        , th [] [ text "Course completed" ]
        ]

courseColumns : Course -> List (Html Msg)
courseColumns course =
    [ td [] [ text course.courseID ]
    , td [] [ text course.ecName ]
    , td [] [ text course.level ]
    , td [] [ text (String.fromInt course.term)]
    , td [] [ text course.timeSlot ]
    , td [] [ text (String.fromInt course.capacity) ]
    , td [] [ input [ type_ "checkbox", checked course.checked] [] ]
    ]

-- ######### END OF VIEW #########

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }