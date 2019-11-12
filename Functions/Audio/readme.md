# Function for audio analysis in R

Most functions are for specific file types (e.g. processing .wav files from Frontier Labs BAR) or with monitoR package.

You can source these from here with something like this (*note this links to the raw version of the file:*):

`source(https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/Audio/splitWav.r)`


### Descriptions

- **audioMeta.r**
Gets meta data from .wav files from filename (FL BAR default file format) and file information. Produces a data frame with the following fields. filename, path, size, recorder id, date start, time start, dateTime start, date end, time end, dateTime end, X, Y, duration. Internal regex could be ajusted to suit different filename formats.


- **splitWav.r**  
Splits .wav files into regular durations (e.g. 15s, 10 m) or between specified times. Designed for filename formats as used by Frontier Labs Bioacoustic Audio Recorder (BAR).
Function will either save new split wavs, or output a dataframe with filenames and from, to, columns. This can then be used to read wav files within specific times, and by intervals with tuneR::readWave



