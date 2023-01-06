# Praat script for a Sound and TextGrid sample with one or two tiers with labelled intervals.
# Outputs the pitch by 10 millisecond increments within each interval according to 
# Speaker Number, Token number, Token (word), Pitch, and Time.
# author Joy Zhong
# Based on a basic script by Daniel Hirst from the website:
# http://uk.groups.yahoo.com/group/praat-users/message/2132
# March 2013

# Slightly modified by Jerry Fine as suggested by Cathryn Yang, June 2020

# Tier1 is the Token (word) label and tier2 is the Speaker Number.
tier1= 1 
# Tier2 is omitted in this modification
# tier2 = 2  

form Input Speaker Number
    positive n 1
endform
label2$ = string$(n)
filename$ = "Speaker" + label2$ + ".txt"

# Pitch constants
# Male parameters = 60-250 Hz
# Female parameters = 100-400 Hz
# some files showed pitch halving/doubling, so we adjust parameters for the following speakers: 

minf0 = 100
maxf0 = 350
silence_threshold = 0.03
voicing_threshold = 0.25

# Measure pitch every 10 milliseconds.
timestep = 1/100

# Clear to begin.
clearinfo   

# Select the Sound and TextGrid sample.
sound = selected("Sound")
textGrid = selected("TextGrid")
select textGrid
nIntervals1 = Get number of intervals... tier1

# Write to a new file.
deleteFile: filename$
fileappend 'filename$' Token_Number 'tab$' Speaker 'tab$' Token 'tab$' Pitch 'tab$' Time 'newline$'
select sound

# Select the pitch.
To Pitch (ac)... 0.0 minf0 15 no silence_threshold voicing_threshold 0.01 0.35 0.14 maxf0
pitch = selected("Pitch")

token = 0     

# Loop through each interval. If that interval has a label, get the tier 1 and tier 2 labels,
# token number, pitch for every 10 ms within that interval, and the corresponding time.

for i to nIntervals1

	select textGrid
	label1$ = Get label of interval... tier1 i  
	# label2$ = Get label of interval... tier2 i omitted -- not required
	
	# Check if interval label is not empty.
	if label1$ != ""
		token += 1

		startTime = Get starting point... tier1 i
		endTime = Get end point... tier1 i
		select pitch

		duration = endTime - startTime
		intervalNumber = duration / timestep
		
		for t from 0 to intervalNumber

			# Increment the time by the timestep.
			time = startTime + t * timestep

			# Get pitch for that time.
			f0 = Get value at time... 'time' Hertz Linear

			# If no pitch is listed for that time, set the pitch to 0.
			if f0 = undefined
				undefined$ = "undefined"
				fileappend 'filename$' 'token' 'tab$' 'label2$' 'tab$' 'label1$' 'tab$' 'undefined$' 'tab$' 'time:2''newline$'

			#If we want to use 0's instead of "undefined" in the case that the pitch isn't listed:
			#if f0 = undefined
			#    	f0 = 0	

			# Append results to the file previously created. 
			else
				fileappend 'filename$' 'token' 'tab$' 'label2$' 'tab$' 'label1$' 'tab$' 'f0:2' 'tab$' 'time:2' 'newline$'
			
			endif

		endfor

	endif

endfor

#select all
#Remove
#exitScript()