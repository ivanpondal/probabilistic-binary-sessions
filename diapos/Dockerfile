FROM ivanpondal/alpine-latex:2.0.1

RUN apk --no-cache add perl wget && \
	tlmgr update --self && \
	tlmgr install stmaryrd txfonts helvetic times bbold todonotes mathpartir pgf blkarray && \
	apk del perl wget
