;; THEOP project;
;; August 2013 version;
;; by Paola Tubaro & Antonio A. Casilli & Yasaman Sarabi;;

;; This version: 16th August 2013;

;; "Privacy On" simulations account for the possibility of agents to change their privacy levels from 0 (always visible) to 1 (visible only to friends) , and vice versa;
;; "Privacy Off" simulations do not allow that: Privacy is always set to 0 (always visible)

;; This version includes the possibility to account for external interventions by network providers to reset privacy to 0
;; to see if behaviours cycle over time
;; this is regulated through the "Interventions?" toggle
;; If it is On, it is assumed that network service provider resets all privacy levels to 0 if they have been above 0.5 for 500 consecutive time steps;
;; If it is Off, there are no such interventions 

;; Depending on the value of the "Sensitivity" parameter, individual privacy choices depend on:
;; - personal network size, or
;; - personal network density (i.e. value of the actor's clustering coefficient)
;; if size is chosen, an agent resets its privacy to 1 if it has links to 3/4 of all agents;
;; if density is chosen, an agent resets its privacy to 1 if its clustering coefficient (CC) is equal to at least 0.75

;; the other key variables are BondBridgThreshold (BB), regulating the type of social capital (bonding or bridging);
;; and Dissonance (D), regulating the degree of openness to cultural and social influence from others.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define global indicators ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 globals [isolates      ; number of nodes with no links to others
          components    ; number of groups of at least two nodes
          density       ; density is number of actual links divided by number of possible links
                        ; the latter is equal to: (number of turtles * number of turtles - 1)/2 
          stationary    ; how many times the model has remained stable, i.e. number of links has not changed
          privacy-alert ; how many times average privacy has remained above its expected value of 0.5
         
          ]
      




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define agent attributes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ; these are the characteristics of each individual
            ; "turtles" are "agents" in Netlogo's jargon
turtles-own [z
            ; z is the vector/list of z1, z2, and z3 (cultural practices)
            ; it is a real value on the [0, 1] interval
            x
            ; x is privacy
            ; it is a binary variable
            a
            ; a is tendency to anomie
              ; it is a real value on the [0, 1] interval
            ; this is to account for different propensity to friendship
            
            ; the following five variables are operational only, and do not have substantive meaning
            group
            ; component to which the turtle belongs
            missed
            ; number of times a turtle has the opportunity to create ties, yet remains isolate
            explored? 
            ; marks turtle as "read" in order to count # of components 
             dz
             ; measures distance between the z of two distinct turtles 
             ; allows accounting for cultural similarities without assuming individual agents are necesarilly identical
             zdist
             ; measures distance between the z of one turtle and reference values for its group (visually = colors)
             node-clustering-coefficient
             ; this is the local density of an agent's personal network
             ; it is computed as number of actual links between its friends, divided by number of possible links between them 
             ; the latter is equal to: (number of friends * number of friends - 1)/2  
             ]




;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; this is step 0 in a simulation run (initialization)
to setup
  
  clear-all ; cancels everything previously existing;
  reset-ticks ; resets time
  clear-all-plots ; cancels all previous plots
  
  ask patches [set pcolor white]
  ; colors screen white to prepare for network visualization
  
  crt #Agents ; create #Agents nodes (number #Agents is user-defined);
  ask turtles [set color black 
               set shape "circle"             
               set size 0.5
               ]
  ;; make them look all the same (black, circles), and smaller than the default value;
  
  ; if privacy settings are on
  ; gives privacy to each turtle
  ; privacy = 0 means seen by all
  ; privacy = 1 means seen only by direct contacts (friends)
  ask turtles [ifelse Privacy? [set x random 2]
                               [set x 0]]

  ; from the viewpoint of the service provider, set up an alert if more than half agents have their privacy settings on
  ifelse mean [x] of turtles <= 0.5
              [set privacy-alert 0]
              [set privacy-alert 1]

    
  ;; take a subset of turtles and 
  ;; create a link between each of them and another one,
  ;; chosen at random from the group;
  ;; provided the turtle can be seen by all (privacy = 0)
  ;; the size of the subset is user-defined
  
  ask n-of #InitialTies turtles [ create-link-with one-of other turtles with [x = 0]]
   
   
  ; count number of network components (= sub-graphs separated from one another)
  ; color each of them differently
  ; plot number and size of components
  detect-components
  color-group
  plot-components
 
  ; layout algorithm for network visualization
  repeat 20 [do-layout]
 

  ; give values of practices z (z1 z2 z3) to each turtle  
  ; based on color (at setup)
  give-z

  ; gives values for tendency to anomie
  ; we take anomie as a randomly distributed variable
  ask turtles [set a random-float 1]

end

   


; procedure called by setup and by go
to detect-components 
 set isolates 0
 set components 0 
 ask turtles [set explored? 0
              set group 0]
 ;; keep exploring till all turtles get explored
  loop
  [
    ;; pick a node that has not yet been explored
    let start one-of turtles with [explored? = 0 ]
    if start = nobody [ stop ]
    ask start [set explored? 1
              
              ; first check if start has any links
              ifelse any? link-neighbors
             
              ; if yes, explore how many nodes can be reached from it
              ; update number of components
              ; and give the turtle a group number
              [set components components + 1
              set group components
              explore]
              
              ;if start has no links, update count of isolates
              ; and leave start in group 0
              ;; nb. group 0 is not a real group or component;
              ;; it is only the category where we put isolates
              [set isolates isolates + 1
              set group 0
              ]]
   ]           
end 

    
; procedure called by detect-components, if not an isolate
; the links of start are given the same group
; and they are asked to explore in turn
to explore
    ask link-neighbors [if explored? = 1 [stop]
                        set explored? 1
                        set group components
                        explore]
end



; procedure called by setup
; at setup, color is attributed depending on group
to color-group
  let i 1
  loop 
  [ask turtles with [group = i]
                  ; define color as a function of the group
                  ; to which a turtle belongs
                  ; unless turtle is an isolate (color = black)
                  ; links are of the same color as the turtles at each end
                  ; color are defined according to the Netlogo range of colors
                  ; if the number of groups is large, the colors are repeated but with different shades
                  ; the higher the group number, the lighter the color shade 
                  [let ccolors remove gray base-colors
                  set color ((item (i - 1 - ((length ccolors) * (int ((i - 1) / (length ccolors))))) ccolors) - (2 * int ((i - 1) / (length ccolors))))
                  ask my-links [set color ((item (i - 1 - ((length ccolors) * (int ((i - 1) / (length ccolors))))) ccolors) - (2 * int ((i - 1) / (length ccolors))))
     ]]                                  
   set i i + 1
   if i > components [stop]
   ]
end
 
 
  
; procedure called by setup and by go
to plot-components
   let groupsize n-values (components) [count turtles with [group = (? + 1)]]
   let sizerank reverse sort groupsize
   set-current-plot "Component Number and Size"
   clear-plot
   set-current-plot-pen "Size"
   let i 1
   if i <= components 
   ; color of component is defined based on the color of member turtles
   ; which in turn depends on their z values
   ; at setup, color, z and group number are basically the same
    [loop
    [let c [color] of one-of turtles with [group = i]
    ifelse (all? turtles with [group = i] [color = c])
        [set-plot-pen-color c]
        ; color is gray if turtles with different colors are in the same component (bridging)
        [set-plot-pen-color gray]
        ; components are in descending order, depending on their size
        plotxy (i) (item (i - 1) sizerank)
        set i i + 1
        if i > components [stop]
    ]]
end                                            



; procedure called by setup
; at setup, z is attributed based on initial color
to give-z
ask turtles [ifelse any? link-neighbors
        [
        ; for non-isolates
        ; define z as the list of z1, z2, z3
        ; set them to 0 to start
        set z (list 0 0 0)
        ; j is a random variable which
        ; determines whether value of a variable is below average (if j = 0)
        ; or above average (j = 1)
        let j random 2
        ; two of the values of z are close to color benchmark
        ; the other value is less close and depends on the Dissonance parameter, which is user-defined
        ifelse j = 0 [foreach [0 1] [set z replace-item ? z max (list ((color / 140) - random-float 0.01) 0)]
                      set z replace-item 2 z max (list ((color / 140) - random-float Dissonance) 0)]
                      
                      [foreach [0 1] [set z replace-item ? z min (list ((group / components) + random-float 0.01) 1)]
                      set z replace-item 2 z min (list ((color / 140) + random-float Dissonance) 1)]
        
        ; now, reshuffle list
        ; so that the potentially outlying value may be on any of the three dimensions
        set z shuffle z
        ]
        
        ; isolates (no link-neighbors) do not have to conform to group values
         ; so their z1 z2 and z3 are attributed randomly
        [set z (list (random-float 1) (random-float 1) (random-float 1))]
        ]
end
                    
                    
                    
; procedure called by setup and by go
to do-layout
    layout-spring turtles links 0.2 1 0.8
    display  ;; so we get smooth animation
end



;;;;;;;;;;;;;;;;;;;;;
;;; Go Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;

; this is a step in the simulation run

to go
 
  ; update tickcount
   tick
   
  ; simulation stops if model has run 1500 times without changes (this is assumed to be the stationary state)
  ; at this point, the model also exports results automatically
  ; the command export-world writes the values of all variables and plots to an external file, in csv format
  ; the name of the file includes the values of Dissonance and BondBridgThreshold being tested, plus the number of ticks necessary to achieve the stationary state
  ; the latter is meant to ensure existing files will not be overwritten (which may still happen)
  if stationary = 1500 [export-world (word "D" Dissonance "_BBT" BondBridgThreshold "_Ticks" ticks ".csv")
                        stop]


  ; If the Interventions? toggle is on, we allow for the network provider to intervene
  ; this means resetting all privacy levels to 0 if they have been above average for long
  if Interventions? [if privacy-alert = 500
                      [ask turtles [set x 0]
                      set privacy-alert 0
                    ]]
                      

  ; this is to count changes (number of links stands for all other changes)
  let t count links

  ; depending on anomie settings, decide whether to form or break a link
  ; depending on individual value of z with respect to color benchmark,
  ; decide whether this will be a bond-type link or a bridge-type link
  ; consequently, update z and if necessary, color
  ; in Privacy-On simulations, also update x
  make-tie-choice

  ; update count of changes

  ; if no change in number of ties, update stationary
  ; if there is a change, reset stationary to 0
  let t' count links
  ifelse t' = t [set stationary stationary + 1
                 set t 0
                 set t' 0
                 ]
                [set stationary 0
                 ]

  ; update privacy alert
  ; this is necessary to establish timing of interventions, if any
  if mean [x] of turtles > 0.5
           [set privacy-alert privacy-alert + 1]

  ; update count of components and isolates
  ; and update group variable
  detect-components
  
  ; update layout
  do-layout     
  ; update plot of component number and size
  plot-components
  ; plot evolution of density, share of isolates, part of homophily
  plot-density
  ; plot values of individual variables
  plot-privacy
  plot-z1
  plot-z2
  plot-z3

end




; procedure called by go
to make-tie-choice

  ; select one turtle randomly
  ; call it "actor"
  let actor one-of turtles
  ; take a random number
  let proba random-float 1

  ask actor [ifelse any? link-neighbors
           
           ; if actor has friends, i.e. link-neighbors (for this to be true, one must have number of components > 0 in the system)
           
           [
           ; define color-based benchmark
           ; and define distance of actor with respect to benchmark as z(dist)= absolute value of [z(actor)-(color reference)]
           ; p.s. benchmark is defined based on color variable, normalized to obtain a real number in the 0-1 interval
            set zdist map [abs (? - (color / 140))] z
           ; this will be used to define whether the tie to be formed or broken will be bond or bridge
           
           ; if proba is higher than a, go to the form-tie procedure
           ; this means that higher tendency to anomie implies lower tendency to form ties
            ifelse proba > a [form-tie]
            ; otherwise, go to the break tie procedure
            ; this means that higher tendency to anomie implies higher tendency to break ties
                                   [break-tie]
           
           ]
           
           ; if actor has no friends, it can only form a new tie by bridging
           ; this will happen if proba is higher than a
           ; otherwise, actor will just update its count of missed opportunities
             [ifelse proba > a [form-bridge]
                               [set missed missed + 1 ]]
 
  ; after making a relational choice, adjust values of z
  ; to fit better with values of friends 
  adjust-z
  ; adjust color too
  adjust-color
  ; calculate clustering coefficient of agent (its local density)
  find-clustering-coefficient
  ; also adjust privacy settings (in Privacy simulations only)
  if Privacy? [adjust-x]]
end


; procedure called by make-tie-choice           
to form-tie
           
           ; first, see if z of actor is close to color benchmark
           ; how close it should be depends on the parameter BondBridgThreshold 
           ; BondBridgThreshold is user-defined
           
           ifelse mean zdist <= BondBridgThreshold
           ; if lower or equal, form a tie with someone in the group and of the same color
           [form-bond]
           ; otherwise, form a bridging tie
           [form-bridge]
           
end




; procedure called by form-tie
to form-bond
   let w who
   let za z
   let g group
   let c color
   let similars other turtles with [(x = 0) and (link-with turtle w = nobody) and (group = g) and (color = c)]
            ; similars are candidates for bonding link formation
            ; they must be visible turtles
            ; should not already have a link with actor
            ; should be of the same group and same color
            ; continue if there is at least one other similar
            
            if any? similars
            ; if there are similars, calculate similarity scores based on values of z
            ; for the three dimensions of z
            ; and take the one with lowest difference from value of actor
            
              [
               foreach sort similars [let ddz (map [abs ((?1) - (?2))] za ([z] of ?))
                
                                      ask ? [set dz min ddz]
                                      ]                      
               let best min-one-of similars [dz]
               ; rename "best" the similar with the closest value of one of the zs
               ; it is the best candidate to new link formation
               create-link-with best
              ]
              
end


; procedure called by form-tie
to form-bridge
   let w who
   let g group
   let za z
   let strangers other turtles with [(x = 0) and (link-with turtle w = nobody) and ((group != g) or (group = 0))]

   ; strangers are isolates and/or those who belong to different components
   ; whose privacy settings allow contact
   ; and who do not already have links with actor
          if any? strangers
          ; continue if there is at least one stranger
         [
          ; if there are strangers, calculate similarity scores based on values of z
          ; for the three dimensions of z
          ; and take the one with lowest difference from value of actor
         foreach sort strangers [let ddz (map [abs ((?1) - (?2))] za ([z] of ?))
               
                                      ask ? [set dz min ddz]
                                ]                        
         let beststr min-one-of strangers [dz]
         ; rename "beststr" the stranger with the closest value of one of the zs
         ; it is the best candidate to new link formation
         create-link-with beststr
         ]
         
end


; procedure called by make-tie-choice
to break-tie
           
           ; first, see if z of actor is close to color benchmark
           ; how close it should be depends on the parameter BondBridgThreshold 
           ; BondBridgThreshold is user-defined
           
           ifelse mean zdist > BondBridgThreshold
           ; if higher or equal, break a tie with someone in the group and of the same color
           [break-bond]
           ; otherwise, break a bridging tie
           [break-bridge]
 end
           


; procedure called by break-tie
to break-bond
       let za z
       let c color
       let closefriends link-neighbors with [(color = c)]
       if any? closefriends
              [foreach sort closefriends  [let ddz (map [abs ((?1) - (?2))] za ([z] of ?))
                                           ask ? [set dz max ddz]]                       
              let worstclosefriend max-one-of closefriends [dz]  
              ; rename "worst" the friend with the most distant value of one of the zs
              ; it is the best candidate to link deletion
              ask link-with worstclosefriend [die]]
end



; procedure called by break-tie
to break-bridge
        let za z
        let c color
        let farfriends link-neighbors with [(color != c) or (color = black)]
        if any? farfriends
             [foreach sort farfriends  [let ddz (map [abs ((?1) - (?2))] za ([z] of ?))
                                        ask ? [set dz max ddz]]                       
             let worstfarfriend max-one-of farfriends [dz]  
             ; rename "worst" the friend with the most distant value of one of the zs
             ; it is the best candidate to link deletion
        ask link-with worstfarfriend [die]]
end



; procedure called by make-tie-choice
to adjust-z
      ; only if actor has friends
        if any? link-neighbors
        ; take average of z values of friends, each dimension separately
        [let meanz (list (mean [item 0 z] of link-neighbors) (mean [item 1 z] of link-neighbors) (mean [item 2 z] of link-neighbors))
        ; measure the difference between the value of actor and the average of friends, for each dimension
        let dmz (map [abs ((?1) - (?2))] z meanz)
        let adjz max dmz
        ; find the maximum of dmz, i.e. the dimension of z for which
        ; the deviation from friends is highest
        let posadjz position adjz dmz
        let diffz ((item posadjz z) - (item posadjz meanz))
        ; on this dimension, if the value of actor is higher than average, reduce it;
        ; otherwise increase it
        ; rate of increase or decrease is given by adjust-z-rate
        ; the latter depends on Dissonance
        let adjust-z-rate (0.01 + random-float (Dissonance - 0.01))
        ifelse diffz > 0 [set z replace-item posadjz z max (list ((item posadjz z) - adjust-z-rate) 0)]
                         [set z replace-item posadjz z min (list ((item posadjz z) + adjust-z-rate) 1)]
         
         ; then check if the change in z requires a change in color
         ]
       
end


; procedure called by adjust-z
to adjust-color
    ; actor can remain of the same color or change
    ifelse any? link-neighbors
    ; this depends on whether or not actor has friends
    [ifelse any? link-neighbors with [color != black]
    ; this depends on whether actor has any links to other actors
    ; who are part of groups rather than former isolates
    [let candcolors (map [[color] of ?] sort link-neighbors with [color != black])
     let diffcolors map [abs ((mean z) - (? / 140))] candcolors  
     let bestcolor min diffcolors
     let posbcolor position bestcolor diffcolors
     ; this will be the color of actor
     set color item posbcolor candcolors
     let c color
     ; update color of links
     foreach sort link-neighbors [ifelse ([color] of ? = c)
                    [ask link-with ? [set color c]]
                    [ask link-with ? [set color gray]]]]

     ; if actor's links are only to former isolates
     ; then convert to the color that is nearest to all
     [let meanz (list (mean [item 0 z] of link-neighbors) (mean [item 1 z] of link-neighbors) (mean [item 2 z] of link-neighbors)) 
      let meanzz mean (map [(?1 + ?2) / 2] z meanz) 
      let diffcolors map [abs (meanzz - (? / 140))] (remove gray base-colors)  
      let bestcolor min diffcolors
      let posbcolor position bestcolor diffcolors
      ; this will be the color of actor
      set color (item posbcolor (remove gray base-colors) - 2 * (meanzz - (bestcolor / 140)))
      let c color
      ; update color of links
      ask my-links [set color c]
      ask link-neighbors [set color c]
      ]
     ]

   ; if actor is an isolate, its color is black
   [set color black]
end 



; the following procedure is called by make-tie-choice
; and leads to update of privacy 
to adjust-x
  
  ; isolates that have missed 10 attempts to form links
  ; will make themselves visible
  if ((missed >= 10) and (x = 1)) [set x 0
                                  set missed 0]
  
  ; connected and visible nodes may make themselves visible to friends only
  ; the conditions under which they do so may be network size or density
  
  if Sensitivity = "Network density" [if node-clustering-coefficient > 0.75 [set x 1]]
  if Sensitivity = "Network Size" [if ((count my-links >= (#Agents * 3 / 4)) and (x = 0)) [set x 1]]
end






;; Clustering computations
;; this procedure is called by make-tie-choice
; to calculate clustering coefficient
to-report in-cluster? [hood]
  report ( member? end1 hood and member? end2 hood )
end


to find-clustering-coefficient

ifelse count link-neighbors <= 1
      [ set node-clustering-coefficient 0 ]
      [
        let hood link-neighbors
        set node-clustering-coefficient (2 * count links with [ in-cluster? hood ] / ((count hood) * (count hood - 1)) )
      ]
  
end




; the following procedure is called by go
; updates value of Density and plots its value
; ticks on x-axis, and Density  + Share isolates on y-axis
to plot-density
   set-current-plot "Density"
   set-current-plot-pen "Density"
   set Density (count links / ((#Agents * (#Agents - 1)) / 2))

   plotxy ticks Density

   set-current-plot-pen "Clustering coefficient"
   plotxy ticks (mean [node-clustering-coefficient] of turtles)
end



; the following procedure is called by go
; plots values of individual variable x, with ticks on x axis
to plot-privacy
   set-current-plot "Average Privacy"
   set-current-plot-pen "Mean x"
   plotxy ticks mean [x] of turtles
end



; the following procedure is called by go
; plots values of individual variable z1, with ticks on x axis
; takes into account both global average and average of groups
to plot-z1
  set-current-plot "z1"
  
  let i 1
    if any? turtles with [group = i] 
    [loop
    [create-temporary-plot-pen (word "Group" i)
    set-plot-pen-mode 2
    let c [color] of one-of turtles with [group = i]
    ifelse (all? turtles with [group = i] [color = c])
    [set-plot-pen-color c]
    [set-plot-pen-color gray]
    plotxy ticks (mean [item 0 z] of turtles with [group = i])
    set i i + 1
    if i > components [stop]
    ]]
end                        
 
 
; the following procedure is called by go
; plots values of individual variable z2, with ticks on x axis
; takes into account both global average and average of groups
to plot-z2
    set-current-plot "z2"
    let i 1
    if any? turtles with [group = i] 
    [loop
    [create-temporary-plot-pen (word "Group" i)
    set-plot-pen-mode 2
    let c [color] of one-of turtles with [group = i]
    ifelse (all? turtles with [group = i] [color = c])
    [set-plot-pen-color c]
    [set-plot-pen-color gray]
    plotxy ticks (mean [item 1 z] of turtles with [group = i])
    set i i + 1
    if i > components [stop]
    ]]
end                        



; the following procedure is called by go
; plots values of individual variable z3, with ticks on x axis
; takes into account both global average and average of groups
to plot-z3
    set-current-plot "z3"
    let i 1
    if any? turtles with [group = i] 
    [loop
    [create-temporary-plot-pen (word "Group" i)
    set-plot-pen-mode 2
    let c [color] of one-of turtles with [group = i]
    ifelse (all? turtles with [group = i] [color = c])
    [set-plot-pen-color c]
    [set-plot-pen-color gray]
    plotxy ticks (mean [item 2 z] of turtles with [group = i])
    set i i + 1
    if i > components [stop]
    ]]
end                        
@#$#@#$#@
GRAPHICS-WINDOW
179
10
618
470
16
16
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
44
17
108
50
Setup
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
45
64
108
97
Go
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
633
19
1040
169
Density
Ticks
NIL
0.0
1000.0
0.0
1.0
true
true
"" ""
PENS
"Density" 1.0 0 -2674135 true "" ""
"Clustering coefficient" 1.0 0 -10141563 true "" ""

SLIDER
10
347
150
380
Dissonance
Dissonance
0
0.1
0.09
0.01
1
NIL
HORIZONTAL

SLIDER
10
310
150
343
BondBridgThreshold
BondBridgThreshold
0
1
0.1
0.1
1
NIL
HORIZONTAL

SWITCH
11
136
149
169
Privacy?
Privacy?
0
1
-1000

SLIDER
9
450
148
483
#InitialTies
#InitialTies
0
#Agents / 2
10
1
1
NIL
HORIZONTAL

PLOT
840
381
1040
531
Average Privacy
Ticks
NIL
0.0
1000.0
0.0
1.0
true
false
"" ""
PENS
"Mean x" 1.0 0 -13345367 true "" ""

SLIDER
9
413
148
446
#Agents
#Agents
0
150
50
10
1
NIL
HORIZONTAL

PLOT
632
381
832
531
Component Number and Size
Components
NIL
0.0
15.0
0.0
10.0
true
false
"" ""
PENS
"Size" 1.0 1 -16777216 true "" ""

PLOT
632
193
832
349
z1
Ticks
NIL
0.0
1000.0
0.0
1.0
true
false
"" ""
PENS
"Mean z1" 1.0 0 -16777216 true "" ""

PLOT
840
193
1040
349
z2
Ticks
NIL
0.0
1000.0
0.0
1.0
true
false
"" ""
PENS
"Mean z2" 1.0 0 -16777216 true "" ""

PLOT
1047
193
1247
349
z3
Ticks
NIL
0.0
1000.0
0.0
1.0
true
false
"" ""
PENS
"Mean z3" 1.0 0 -16777216 true "" ""

CHOOSER
10
186
149
231
Sensitivity
Sensitivity
"Network Size" "Network density"
1

SWITCH
11
243
147
276
Interventions?
Interventions?
0
1
-1000

@#$#@#$#@
WHAT IT IS

This model has been developed by Paola Tubaro, Antonio A. Casilli and Yasaman Sarabi as part of the THEOP project, funded by Fondation CIGREF in 2011-2012. It accompanies the book "AGAINST THE HYPOTHESIS OF THE END OF PRIVACY: An Agent-Based Modeling Approach to Social Media", Springer, SpringerBriefs in Digital spaces series (in press).

The model has been realised in Netlogo (Wilensky 1999). 

The model is released under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License (cf. http://creativecommons.org/licenses/by-nc-sa/3.0/).

THEOP sets out to study the so-called “End of Privacy” hypothesis according to which today’s pervasive computer-mediated communication generates a tendency, especially among younger generations, to renounce the value of privacy in favour of an open, connected existence. THEOP aims to identify, in theoretical perspective, the conditions that may actually bring this scenario into being.



HOW IT WORKS

The model represents an online social network in which agents form, maintain and delete online ties to build their social capital, while having to disclose some of their personal information to better interact with others. There is a tension between the need to disclose more to form better social capital, and the need to keep one's information private.

The model also accounts for the fact that the processes of selection (choice of contacts/friends) and social influence (behaviour change induced by relationships with contacts/friends) in a social network also affect privacy choices, and are in turn affected by them. Selection determines to whom contents are disclosed, while influence determines what is disclosed, in a dynamic process with feedback. 

The model consists of a population of agents operating in an environment, mimicking an online social networking service.

The environment is characterised by two main features:

 - Dissonance (D), indicates openness to social influence by others.
 - Bonding / Bridging threshold (BB), indicates the extent to which bonding ties (with close others) are preferred to bridging ties in the creation of social capital.

Two more minor features (with few effects on model results) are the number of agents (assumed fixed during a simulation run) and the number of ties at initialisation.

The environment also includes a set of options:
 - "Privacy-On" vs. "Privacy-Off": in the former case, simulations account for the possibility of agents to change their privacy levels from 0 (always visible to all) to 1 (visible only to direct contacts), and vice versa; in the latter case, Privacy is always set to 0 (always visible).
 - Possibility of "Privacy Interventions": if present, the network service provider resets all privacy levels to 0 if mean privacy has been above 0.5 for a long time; if absent, there are no such interventions.
 - "Sensitivity", that is, in Privacy-On simulations, what network structural aspect induces agents to react by resetting their privacy levels from 0 to 1: in the case of "Network size", an agent resets its privacy to 1 if it has links to 3/4 of all agents; in the case of "Network density", an agent resets its privacy to 1 if its local density (measured through its clustering coefficient) is equal to at least 0.75.

All these parameters are user-defined and do not change in the course of a simulation run.

In turn, individual agents are characterised by two variables:
 - Privacy setting (binary, taking value 0 or 1);
 - Cultural practices (a vector with three dimensions, here labelled z1, z2 and z3; each is a continuous variable defined on the interval [0; 1]).
The values of these variables are assigned randomly at initialisation, and evolve endogenously during the simulation.

 

THINGS TO TRY

Choose your preferred simulation parameters and press the "Setup" button to initialize the model. You will see the initial structure of the network in the main View panel of the Interface.

Then, press the "Go" button to start a simulation run. The system stops automatically once it reaches the stationary state; at that moment, it also exports all values of variables and constants. If you wish to stop it earlier, press the "Go" button again.

While the simulation is running, you can see the evolution of the network structure in the main view, and follow changes in key indicators in the plots situated to the right of the screen:
 - Global network density and local density (as measured by the clustering coefficient, an average of the density of the personal networks of agents);
 - Values of the cultural practices of agents z1, z2 and z3 (group averages);
 - Number and size of network components;
 - Average level of privacy in the system.  



REFERENCES
Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University. Evanston, IL. 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
