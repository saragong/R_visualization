#### convert.R ####
#### convert to long panel format

all_waves_long <- long_panel(all_waves, 
                             prefix = "_WAVE_", 
                             periods=c("A", "B", "C", "D", "E", "F"),
                             label_location = "end")



# end
