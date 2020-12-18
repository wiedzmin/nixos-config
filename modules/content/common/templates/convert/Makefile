yt/download:
	youtube-dl -o "%(title)s.%(ext)s" `xsel -b -o`

concat:
	echo "not implemented/tested" # ffmpeg -f concat -i files.list -c copy result.mp4

mp4:
	for i in *.MTS; do ffmpeg -i "$i" -c:v copy -c:a copy -b:a 192k "$i.mp4"; done

audio:
	mkdir -p result
	for i in *.mp4; do ffmpeg -i "$$i" -codec:a libmp3lame "result/$${i%.*}.mp3"; done
	for i in *.mkv; do ffmpeg -i "$$i" -codec:a libmp3lame "result/$${i%.*}.mp3"; done
	for i in *.webm; do ffmpeg -i "$$i" -codec:a libmp3lame "result/$${i%.*}.mp3"; done

clean:
	rm -rf ./result
