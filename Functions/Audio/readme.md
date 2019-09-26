# Function for audio analysis in R

Most functions are for specific file types (e.g. processing .wav files from Frontier Labs BAR) or with monitoR package.

You can source these from here with something like this (*note this links to the raw version of the file:*):

`https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/Audio/splitWav.r`


### Descriptions

- **splitWav.r**  
Splits .wav files into regular durations (e.g. 15s, 10 m) or between specified times. Designed for filename formats as used by Frontier Labs Bioacoustic Audio Recorder (BAR).
Function will either save new split wavs, or output a dataframe with filenames and from, to, columns. This can then be used to read wav files within specific times, and by intervals with tuneR::readWave

