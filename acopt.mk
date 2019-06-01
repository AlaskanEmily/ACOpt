
all: acopt

MMCOPTS=--use-grade-subdirs -O 6

LIBJSONDIR=json/src
LIBJSONFLAGS=--search-lib-files-dir "$(LIBJSONDIR)" --init-file "$(LIBJSONDIR)/json.init" --link-object "$(LIBJSONDIR)/libjson.$(LIBSUFFIX)"

mercury_json:
	cd json && cd src && $(MMC) --make libjson $(MMCOPTS)

acopt: mercury_json
	$(MMC) $(MMCOPTS) $(LIBJSONFLAGS) --make acopt

clean: acopt_clean mercury_json_clean
	$(DELETE) *.exe || $(NOTHING)
	$(DELETE) acopt || $(NOTHING)
	$(DELETE) *.err || $(NOTHING)
	$(DELETE) *.mh  || $(NOTHING)

acopt_clean:
	$(MMC) $(MMCOPTS) $(LIBJSONFLAGS) --make acopt.clean || $(NOTHING)
	$(RMDIR) Mercury || $(NOTHING)

mercury_json_clean:
	cd $(LIBJSONDIR) && $(MMC) $(MMCOPTS) $(LIBJSONFLAGS) --make libjson.clean || $(NOTHING)
	cd $(LIBJSONDIR) && $(MMC) $(MMCOPTS) $(LIBJSONFLAGS) --make json.clean || $(NOTHING)
	cd $(LIBJSONDIR) && $(MMC) $(MMCOPTS) $(LIBJSONFLAGS) --make json.clean || $(NOTHING)
	$(DELETE) *.err || $(NOTHING)
	$(DELETE) *.mh  || $(NOTHING)
	cd $(LIBJSONDIR) && $(RMDIR) Mercury || $(NOTHING)
 