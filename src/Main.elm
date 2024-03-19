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

type alias Course = 
    {   term : Int,
        timeSlot : String,
        courseID : String,
        level : String,
        ecName : String,
        capacity : Int
    }

type alias Model =
    { courses : List Course
    , error : Maybe String
    , content : String
    , prerequisites : List Course
    }

type Msg
    = FetchCourses
    | ReceiveCourses (Result Http.Error (List Course))
    | Change String
    | FetchPrerequisites String
    | ReceivePrerequisites (Result Http.Error (List Course))

init : Model
init = { courses = [], error = Nothing, content = "", prerequisites = []}

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

coursesDecoder : Decoder (List Course)
coursesDecoder =
    Decode.list courseDecoder

courseDecoder : Decoder Course
courseDecoder =
    Decode.map6 Course
        (field "term" Decode.int)
        (field "timeSlot" Decode.string)
        (field "courseID" Decode.string)
        (field "level" Decode.string)
        (field "ecName" Decode.string)
        (field "capacity" Decode.int)

view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Search", value model.content, onInput Change ] []
        , button [ onClick FetchCourses ] [ text "Fetch Courses" ]
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
        [ td [] [ text (String.fromInt course.term)]
        , td [] [ text course.timeSlot ]
        , td [] [ text course.courseID ]
        , td [] [ text course.level ]
        , td [] [ text course.ecName ]
        , td [] [ text (String.fromInt course.capacity) ]
        ]
    ]

prereqRows : List Course -> List (Html Msg)
prereqRows prerequisites =
    List.concatMap prereqRow prerequisites

prereqRow : Course -> List (Html Msg)
prereqRow course =
    [ tr []
        [ td [] [ text (String.fromInt course.term)]
        , td [] [ text course.courseID ]
        , td [] [ text course.level ]
        , td [] [ text course.ecName ]
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
                ]
            ]
        , tbody [] (prereqRows prerequisites)
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, fetchCourses)
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }