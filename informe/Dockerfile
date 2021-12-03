FROM ivanpondal/alpine-latex:2.0.1

RUN apk --no-cache add perl wget && \
	tlmgr install stmaryrd txfonts helvetic times bbold todonotes && \
	apk del perl wget
