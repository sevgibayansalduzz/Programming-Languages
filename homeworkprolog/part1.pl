:- dynamic student/3.
:- dynamic course/7.
:- dynamic room/4.
student(1,[cse341,cse343,cse331],0).
student(2,[cse341,cse343],0).
student(3,[cse341,cse331],0).
student(4,[cse341],0).
student(5,[cse343,cse331],0).
student(6,[cse341,cse343,cse331],1). % 1 represents a handicapped student .
student(7,[cse341,cse343],0). % 0 represents a student who is not handicapped.
student(8,[cse341,cse331],1).
student(9,[cse341],0).
student(10,[cse341,cse321],0).
student(11,[cse341,cse321],0).
student(12,[cse343,cse321],0).
student(13,[cse343,cse321],0).
student(14,[cse343,cse321],0).
student(15,[cse343,cse321],1).

instr(genc,cse341,projector).
instr(turker,cse343,smartboard).
instr(bayrakci,cse331,_).
instr(gozupek,cse321,smartboard).

course(cse341,genc,10,4,z06,[1,2,3,4,6,7,8,9,10,11],_).% "_" represents the needs column for course table which is in the data.xls
course(cse343,turker,10,3,z11,[1,2,5,6,7,12,13,14,15],_).
course(cse331,bayrakci,6,3,z06,[1,3,5,6,8],_).

room(z06,10,hcapped,projector).
room(z11,10,hcapped,smartboard).

occupancy(z06,8,cse341).
occupancy(z06,9,cse341).
occupancy(z06,10,cse341).
occupancy(z06,11,cse341).
occupancy(z06,12,empty).
occupancy(z06,13,cse331).
occupancy(z06,14,cse331). 
occupancy(z06,15,cse331).
occupancy(z06,16,empty).
occupancy(z11,8,cse343).
occupancy(z11,9,cse343).
occupancy(z11,10,cse343).
occupancy(z11,11,cse343).
occupancy(z11,12,empty).
occupancy(z11,13,empty).
occupancy(z11,14,cse321).
occupancy(z11,15,cse321).
occupancy(z11,16,cse321).


newroom(X,Y,Z,T) :- assertz(room(X,Y,Z,T)).
newstudent(X,Y,Z) :- assertz(student(X,Y,Z)).
newcourse(X,Y,Z,T) :- assertz(course(X,Y,Z,T,_,_,_)).
%-------------------
%Check whether there is any scheduling conflict. 
conflict(CourseID1,CourseID2):- occupancy(_,Time,CourseID1),%take the time of CourseID1
    							occupancy(_,Time,CourseID2)%take the time of CourseID2
    							,not(CourseID1==CourseID2),!.%if there is any match return true.Otherwise retunr false.

%-------------------
% Check which room can be assigned to a given class. 

%if there is a student who is handicapped make Control 1 ,other wise make 0.
checkStudentNeed(CourseID,Control):-student(_,List,Hc),member(CourseID,List),(Hc = 1,Control is 1;Control is 0),!.

assign(RoomID,CourseID):- instr(_,CourseID,Need),%keep the instructor`s need in to the Need
    						course(CourseID,_,Cap,_,_,_,Need),
    						checkStudentNeed(CourseID,Control),%Find the Control for Students`s need.
    (Control is 1,room(RoomID,Caproom,hcapped,Need);Control is 0,room(RoomID,Caproom,_,Need))
    %if Control is 1,it means there is an handicapped student.Thus call the room with 'hcapped'.It will check the room for that is appropriate to handicapped students.
    %if Contol is 0,it means there is no handicapped students.Thus call the room with'_'.It will not check the room according to handicapped`s appropriate.
    ,Cap=<Caproom.   %compare the room`s capacity and  the course`s capacity.

%-------------------
%Check which room can be assigned to which classes. 
assign2(RoomID,List):-findall(Course,assign(RoomID,Course),List).%call the rule assign and list results .

%-------------------
%Check whether a student can be enrolled to a given class. 
enr(_,[]).
enr(CourseID,[Head|Tail]) :- not(conflict(CourseID,Head)),%compare the given course`s hour with student`s courses.Check whether there is any conflict. 
    								not(CourseID==Head),%if the student is already taking the given course,do not enroll.
    								course(CourseID,_,Cap,_,_,L,_),
    								length(L,Len),Len < Cap,%Calculate the number of students enrolled in the given course and compare it with the capacity of the given course.
    								enr(CourseID,Tail).%make recursive call the courses of the students.
enroll(StudentID,CourseID):-student(StudentID,List,_),%take the list of the course of the student according to studentID.
    						enr(CourseID,List),!.
%-------------------
%Check which classes a student can be assigned. 
%
%The  rule enroll2 is same above rule.Becouse of "!" which is in the above rule,I cant list results so i rewrite the predicates. 
rec(_,[]).
rec(Course,[Head|Tail])	:-not(conflict(Course,Head)),%compare the given course`s hour with student`s courses.Check whether there is any conflict. 
    					not(Course==Head),rec(Course,Tail).%if the student is already taking the given course,do not enroll.Make recursive call.

enroll2(StudentID,List):-findall(Course,
                                 (student(StudentID,List2,_),%take the list of the course of the student according to studentID.
                                % (NeedH =0,room(Room,_,_,_);NeedH=1,room(Room,_,hcapped,_)),
                                course(Course,_,Cap,_,_,L,_),
    							length(L,Len),Len < Cap,%Calculate the number of students enrolled in the given course and compare it 
                                rec(Course,List2)		
                                 )
                                 ,List).
%%%%%%%%%%%%%     TESTS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%test samples for conflict.

%conflict(cse341,cse331).% it will retur false
%conflict(cse341,cse321).%it will return false
%conflict(cse341,cse343).%true
%conflict(cse321,cse331).%true
%conflict(cse343,cse331).%false


%test samples for assign.

%assign(Room,cse341).%it will return z06.Because the instructor`s need is projector and there is/are handicapped student(s). 
%assign(Room,cse343).%it will return z11.Because the instructor`s need is smartboard and there is/are handicapped student(s).
%assign(Room,cse331).%it will return z11 or Z06.Because the instructor`s need is nothing and there is/are handicapped student(s).
%assign(Room,cse321).%it will return z11.Because the instructor`s need is smartboard and there is/are handicapped student(s).

%test samples for assign2.
%assign2(z11,List).%it will return [cse343, cse331, cse321] because this courses needs smartboard or nothing.
%assign2(z06,List).%it will return [cse341, cse331] because this courses needs projector or nothing.


%test samples for enroll.
%enroll(15,cse341).% it will return false because there is an scheduling conflict and also there is no capacity for cse341.
%enroll(9,cse331).% it will return true
%enroll(9,cse343).%it will return false because there is an scheduling conflict 


%test samples for enroll2.
%enroll2(1,L).%it will return empty list because there is no appropriate course.
%enroll2(2,L).%it will return [cse331, cse321] ;because this student`s course are cse341 and cse343,this two course is between 8 am-11am.
%Thus this rule return the list of the courses which is afternoon.

%enroll2(3,L).%it will return empty list because there is no appropriate course.

%enroll2(4,L).%it will return [cse331, cse321] ;because this student`s course is cse341 and this course is between 8 am-11am.
%Thus this rule return the list of the courses which is afternoon.

%enroll2(5,L).%it will return empty list because there is no appropriate course.
%enroll2(10,L).%it will return empty list because there is no appropriate course.
%enroll2(13,L).%it will return empty list because there is no appropriate course.