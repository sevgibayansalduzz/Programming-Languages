flight(edirne,erzurum,5).
flight(erzurum,edirne,5).

flight(erzurum,antalya,2).
flight(antalya,erzurum,2).

flight(antalya,diyarbakır,5).
flight(diyarbakır,antalya,5).

flight(antalya,izmir,1).
flight(izmir,antalya,1).

flight(diyarbakir,ankara,8).
flight(ankara,diyarbakir,8).

flight(izmir,ankara,6).
flight(ankara,izmir,6).

flight(izmir,istanbul,3).
flight(istanbul,izmir,3).

flight(istanbul,ankara,2).
flight(ankara,istanbul,2).

flight(istanbul,trabzon,3).
flight(trabzon,istanbul,3).

flight(trabzon,ankara,6).
flight(ankara,trabzon,6).

flight(ankara,kars,3).
flight(kars,ankara,3).

flight(kars,gaziantep,3).
flight(gaziantep,kars,3).


route(X,Y,C) :- flight(X,Y,C); % a predicate indicating there exist a route between X and Y if there is flight between X and Y with cost C.
				route(X,Y,C,[]). %calculates the alternative paths between X and Y with calling the four parameter route rule.

route(X,Y,C,_) :- flight(X,Y,C).%Base case for recursive function: if List is empty 

route(X,Y,C,L) :- not(member(X,L)),%if X is in the L,that means you already passed from X city ,then try another city. Otherwise use this city.
    						flight(X,Z,A), 	%find an altenative path.
							route(Z,Y,B,[X|L]), %make a recursive call
    						not(X=Y),%if destination point and starting point are not same,add A+B into the C(Cost).
    						C is A + B.		