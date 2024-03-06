URL endpoints: 
    /courses --all courses
    /courses/courseID --specific course
    /courses/courseID/prereq -- show prerequisites 

Data type: 
    Data Course = {term, timeslot, courseID, level, EC Name, Capacity}
    Data CourseOverview = [(courseID, courseID)]

Idea:
    show only courses that you haven't already done. Like a filter that is given to the endpoint to cut out those courses.

Stijn: Type definitions
Alex: Function for types
Martin: URL endpoints

Week 11
Martin: Elm Visualize the information. Start with core features.
Alex: Servant swagger for documentation including behavior that is expected from endpoints in for example status codes, working on functionality
Stijn: Status report and finalizing types
