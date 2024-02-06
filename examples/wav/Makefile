WAV = $(wildcard *.wav)
MP3 = $(WAV:.wav=.mp3)
MP4 = $(WAV:.mp3=.mp4)

all: $(MP4)

clean:
	rm -f $(MP4)

%.mp3: %.wav
	lame --abr 160 $<

%.mp4: %.mp3
	ffmpeg -i $< -filter_complex "[0:a]showspectrum=s=1280x720:color=intensity[v]" -map "[v]" -map 0:a -pix_fmt yuv420p $@
