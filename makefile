LOAD_PATH = -eval '(setq load-path (append (list (expand-file-name ".") (expand-file-name "lib" ".")) load-path))'
EMACS = emacs $(LOAD_PATH)
FLAGS = -batch -q -no-site-file -l $(srcdir)/dgnushack.el
FILES = rtf-controls.elc rtf-reader.elc rtf-par.elc rtf.elc
FLAGS = -batch -f batch-byte-compile

all: $(FILES)

rtf.elc: 
	$(EMACS) -l "rtf-reader" $(FLAGS) rtf.el

rtf-controls.elc:
	$(EMACS) -l "rtf-reader" $(FLAGS) rtf-controls.el

rtf-reader.elc: rtf-controls.elc
	$(EMACS) -l "rtf" -l "state-m" -l "rtf-controls" $(FLAGS) rtf-reader.el

rtf-par.elc:
	$(EMACS) $(FLAGS) rtf-par.el

clean:
	rm -f *.elc
