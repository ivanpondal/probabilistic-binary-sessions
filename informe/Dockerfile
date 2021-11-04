FROM ivanpondal/alpine-latex:2.0.1

RUN apk --no-cache add perl wget && \
	tlmgr install txfonts helvetic times bbold && \
	apk del perl wget
