;;; ox-report.el --- Export your org file to minutes report PDF file -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Matthias David
;; Author: Matthias David <db@gnu.re>
;; URL: https://github.com/DarkBuffalo/ox-report
;; Version: 0.5
;; Package-Requires: ((emacs "24.4") (org-msg "3.9"))
;; Keywords: org, outlines, report, exporter, meeting, minutes

;;; Commentary:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This is a another exporter for org-mode that translates Org-mode file to
;; beautiful PDF file
;;
;; EXAMPLE ORG FILE HEADER:
;;
;;   #+title:Readme ox-notes
;;   #+author: Matthias David
;;   #+options: toc:nil
;;   #+ou:Zoom
;;   #+quand: 20/2/2021
;;   #+projet: ox-minutes
;;   #+absent: C. Robert,T. tartanpion
;;   #+present: K. Soulet,I. Payet
;;   #+excuse:Sophie Fonsec,Karine Soulet
;;   #+logo: logo.png
;;
;;; Code:

(require 'ox)
(require 'cl-lib)
(require 'org-msg)
;; Optional dependencies
(require 'ox-odt nil t)  ; Pour l'export ODT (optionnel)


(defgroup ox-report nil
  "Export your org file to minutes report PDF file."
  :group 'ox)

(defcustom ox-report-logo nil
  "Logo path."
  :type 'path
  :group 'ox-group)

(setq org-latex-packages-alist
      '(("AUTO" "babel" t ("pdflatex"))
        ("utf8" "inputenc" t ("pdflatex" "xelatex" "lualatex"))
        ("T1" "fontenc" t ("pdflatex" "xelatex" "lualatex"))))

(add-to-list 'org-latex-classes
             '("report" ;;class-name
               "\\documentclass[10pt]{article}

\\RequirePackage{iflang}
\\RequirePackage{setspace}                    %%pour le titre
\\RequirePackage{graphicx}	              %% gestion des images
\\RequirePackage[dvipsnames,table]{xcolor}    %% gestion des couleurs
\\RequirePackage{array}		              %% gestion améliorée des tableaux
\\RequirePackage{calc}		              %% syntaxe naturelle pour les calculs
\\RequirePackage{enumitem}	              %% pour les listes numérotées
\\RequirePackage[footnote]{snotez}	      %% placer les notes de pied de page sur le coté
\\RequirePackage{microtype,textcase}
%%\\RequirePackage{titlesec}
\\RequirePackage{booktabs}

\\RequirePackage{amsmath, amssymb, amsthm}   %% For including math equations, theorems, symbols, etc
\\RequirePackage[toc]{multitoc}
\\RequirePackage[a4paper,left=15mm,
top=15mm,headsep=2\\baselineskip,
textwidth=132mm,marginparsep=8mm,
marginparwidth=40mm,textheight=58\\baselineskip,
headheight=\\baselineskip]{geometry}

\\microtypesetup{protrusion=true,final}
%%----------------------------------------------------------------------------------------
%%	HEADERS
%%----------------------------------------------------------------------------------------
\\makeatletter

\\newenvironment{fullpage}
    {\\noindent\\begin{minipage}
    {\\textwidth+\\marginparwidth+\\marginparsep}\\smallskip}
    {\\end{minipage}
%%\\vspace{2mm}
}


%% COLOR %<--------------------------------------------------------->%
\\RequirePackage{xcolor}

%% Contrast colours
\\definecolor{mdgreen}{HTML}{A4C21F}

%% Additional colours
\\definecolor{mdgrey}{HTML}{A19589}
\\colorlet{mdgray}{mdgrey}
\\definecolor{mdlightgrey}{HTML}{D8D0C7}
\\colorlet{mdlightgray}{mdlightgrey}


%% DOC %<----------------------------------------------------------->%

\\ProcessOptions\\relax

%% Command to provide alternative translations
\\newcommand{\\UseLanguage}[3]{
   \\IfLanguageName{french}{#1}{}
   \\IfLanguageName{english}{#2}{}
   \\IfLanguageName{american}{#2}{}
   \\IfLanguageName{british}{#2}{}
   \\IfLanguageName{german}{#3}{}
}

%% This} separating line is used across several documents,
\\newcommand{\\@separator}{%%
 %% To make sure we have spacing on both sides, make an invisible rule, 2X tall
  \\rule{0ex}{2ex}%%
   %% Place the dashed rule 1X high
  \\textcolor{mdgray}{\\rule[1ex]{\\textwidth}{0.25pt}}%%
}


%% LABEL %-------------------------------------------------------%
%% Standard style for labels, small and bold
\\newcommand{\\@labeltext}{\\large\\scshape}

\\newcommand*{\\@approvedlabel}{\\UseLanguage{APPROUVE PAR}{APPROVED BY}{FREIGEGEBEN VON}}
\\newcommand*{\\@approved}{Set with \\texttt{\textbackslash approved\\{\\}}}
\\newcommand*{\\approved}{\\renewcommand*{\\@approved}}

\\newcommand*{\\@authorlabel}{\\UseLanguage{Auteur(s)}{Author(s)}{Author(en)}}
\\newcommand*{\\@Authorlabel}{\\UseLanguage{AUTEUR(S)}{AUTHOR(S)}{AUTHOR(EN)}}

\\newcommand*{\\@checkedlabel}{\\UseLanguage{VERIFIE PAR}{CHECKED BY}{VERIFIZIERT DURCH}}
\\newcommand*{\\@checked}{Set with \\texttt{\textbackslash checked\\{\\}}}
\\newcommand*{\\checked}{\\renewcommand*{\\@checked}}

\\newcommand*{\\@datelabel}{\\UseLanguage{DATE}{DATE}{DATUM}}
\\newcommand*{\\@absentlabel}{\\UseLanguage{ABSENT}{ABSENT}{ABWESEND}}
\\newcommand*{\\@excusedlabel}{\\UseLanguage{EXCUSE}{EXCUSED}{ENTSCHULDIGT}}

\\newcommand*{\\@durationlabel}{\\UseLanguage{DUREE}{DURATION}{DAUER}}
\\newcommand*{\\@duration}{Set with \\texttt{\\textbackslash duration\\{\\}}}
\\newcommand*{\\duration}{\\renewcommand*{\\@duration}}

\\newcommand*{\\@initiatorlabel}{\\UseLanguage{INITIATEUR}{INITIATED BY}{INITIATOR}}
\\newcommand*{\\@initiator}{Set with \\texttt{\\textbackslash initiator\\{\\}}}
\\newcommand*{\\initiator}{\\renewcommand*{\\@initiator}}

\\newcommand*{\\@participantlabel}{\\UseLanguage{PARTICIPANT}{PARTICIPANT}{TEILNEHMER}}
\\newcommand*{\\@participantslabel}{\\UseLanguage{PARTICIPANTS}{PARTICIPANTS}{TEILNEHMER}}

\\newcommand*{\\@preparedlabel}{\\UseLanguage{PREPARE PAR}{PREPARED BY}{VORBEREITET VON}}
\\newcommand*{\\@prepared}{Set with \\texttt{\\textbackslash prepared\\{\\}}}
\\newcommand*{\\prepared}{\\renewcommand*{\\@prepared}}

\\newcommand*{\\@presentlabel}{\\UseLanguage{PRESENT}{PRESENT}{ANWESEND}}

\\newcommand*{\\@projectlabel}{\\UseLanguage{PROJET}{PROJECT}{PROJEKT}}
\\newcommand*{\\@project}{Set with \\texttt{\\textbackslash project\\{\\}}}
\\newcommand*{\\project}{\\renewcommand*{\\@project}}

\\newcommand*{\\@referencelabel}{\\UseLanguage{REFERENCE}{REFERENCE}{REFERENCE}}
\\newcommand*{\\@reportlabel}{\\UseLanguage{Rapport}{Report}{Bericht}}

\\newcommand*{\\@reportnumberlabel}{\\UseLanguage{RAPPORT N°}{REPORT NUMBER}{BERICHT NUMMER}}
\\newcommand*{\\@reportnumber}{Set with \\texttt{\\textbackslash reportnumber\\{\\}}}
\\newcommand*{\\reportnumber}{\\renewcommand*{\\@reportnumber}}

\\newcommand*{\\@wheremeeting}{Set with \\texttt{\\textbackslash wheremeeting\\{\\}}}
\\newcommand*{\\wheremeeting}{\\renewcommand*{\\@wheremeeting}}

\\newcommand*{\\@whenmeeting}{Set with \\texttt{\\textbackslash whenmeeting\\{\\}}}
\\newcommand*{\\whenmeeting}{\\renewcommand*{\\@whenmeeting}}

%% TASKS
\\newcommand*{\\@tasklistlabel}{\\UseLanguage{Liste de tâches}{Task List}{Aufgaben}}
\\newcommand*{\\@tasknumberlabel}{\\#}
\\newcommand*{\\@tasklabel}{\\UseLanguage{TACHE}{TASK}{Aufgabe}}
\\newcommand*{\\@duelabel}{\\UseLanguage{DATE D'ECHEANCE}{DUE DATE}{ERLEDIGUNGSDATUM}}
\\newcommand*{\\@responsiblelabel}{\\UseLanguage{RESPONSABLE}{RESPONSIBLE}{VERANTWORTLICH}}

%% MINUTES %-------------------------------------------------------%
\\ProcessOptions\\relax

\\PassOptionsToPackage{table}{xcolor}

\\renewcommand*{\\@authorlabel}{\\UseLanguage{ECRIT PAR}{WRITTEN BY}{GESCHRIEBEN VON}}


%% Setting up header and footer
\\RequirePackage{fancyhdr,lastpage}
\\pagestyle{fancy}

%% Header
\\fancyhead{}
\\renewcommand{\\headrulewidth}{0pt}

%% Footer
\\renewcommand{\\footrulewidth}{0pt}
\\fancyfoot[c]{%%
  \\sffamily%%
  \\color{mdgray}
  \\@separator\\newline
  ~~%%
  \\begin{minipage}[c]{0.5\\textwidth}
    \\hspace*{3pt}\\small{\\textbf{\\@projectlabel}}\\newline
    \\hspace*{\\tabcolsep}\\@project
  \\end{minipage}%%
  \\hfill
  \\thepage\\ \\UseLanguage{de}{of}{von} \\pageref{LastPage}
  ~~\\newline
  \\@separator
}

%% The logo box.
\\newcommand{\\@rlogo}{
  \\noindent
  \\scriptsize
  \\raggedleft
  \\setlength{\\parskip}{1ex}
  \\includegraphics[height=70px,width=70px,keepaspectratio]{\\@mainlogo}
%%\\includegraphics[width=\\textwidth]{\\@mainlogo}
}

\\RequirePackage{xparse}
\\newcommand{\\@participantstable}{}
\\NewDocumentCommand \\participant { O{present} m }{
    \\g@addto@macro \\@participantstable {
        \\multicolumn{2}{l}{#2}
          & \\ifstrequal{#1}{present}    {$\\bullet$}{}
          & \\ifstrequal{#1}{absent}     {$\\bullet$}{}
          & \\ifstrequal{#1}{excused}    {$\\bullet$}{}\\\\
    }
}

\\RequirePackage{tabularx,ltxtable}
\\newcommand{\\@tasktable}{}
\\newcommand{\\tasklist}{%%
  \\section*{\\@tasklistlabel}
  \\vspace{-\\baselineskip}
  \\begin{longtable}{rp{0.55\\textwidth}p{0.2\\textwidth}l}
    \\multicolumn{4}{@{}c@{}}{\\@separator}\\\\*
    \\@labeltext \\@tasknumberlabel & \\@labeltext \\@tasklabel &
    \\@labeltext \\@responsiblelabel & \\@labeltext \\@duelabel\\\\*
    \\multicolumn{4}{@{}c@{}}{\\@separator}
    \\@tasktable\\\\*
  \\end{longtable}
}
\\newcounter{sinteftask}
\\newcommand{\\task}[3]{%%
    \\g@addto@macro \\@tasktable {%%
      \\\\
      \\refstepcounter{sinteftask}\\thesinteftask & #1 & #2 & #3 \\\\*
      \\multicolumn{4}{@{}c@{}}{\\@separator}%%
    }%%
}

%% Recipient address and information colophon
\\RequirePackage{colortbl,tabularx,rotating}
\\newcommand{\\frontmatter}{%%
  \\sffamily%%
  \\noindent%%
  \\begin{minipage}[b]{0.7\\textwidth}
    \\setlength{\\parskip}{2ex}%%
    \\huge\\textbf\\@title

    %% ~ ensures \\ does not crash when \@wheremeeting is empty
    \\Large \\@wheremeeting~\\\\\\@whenmeeting
  \\end{minipage}
  \\hfill
  \\begin{minipage}[b]{0.20\\textwidth}
    %% Bring the colophon and address back up a bit
    \\vspace*{-25pt}
   \\@rlogo
  \\end{minipage}

  \\vspace{1ex}%%
  \\noindent%%
  \\@separator\\\\
  \\rowcolors{4}{}{mdlightgray}
  \\begin{tabularx}{\\textwidth}{XXccc}
    \\rowcolor{white}
      \\parbox{\\linewidth}{{\\@labeltext \\@initiatorlabel}\\\\\\@initiator}
      & \\parbox{\\linewidth}{{\\@labeltext \\@authorlabel}\\\\\\@author}
      & \\raisebox{-1cm}{\\begin{sideways}\\parbox{2cm}{\\raggedright\\@labeltext\\@presentlabel}\\end{sideways}}
      & \\raisebox{-1cm}{\\begin{sideways}\\parbox{2cm}{\\raggedright\\@labeltext\\@absentlabel}\\end{sideways}}
      & \\raisebox{-1cm}{\\begin{sideways}\\parbox{2cm}{\\raggedright\\@labeltext\\@excusedlabel}\\end{sideways}}\\\\
    \\rowcolor{white} \\multicolumn{5}{@{}c@{}}{\\@separator}\\\\
    \\rowcolor{white} \\@labeltext \\@participantslabel\\\\
    \\@participantstable
  \\end{tabularx}

  \\rowcolors{1}{}{} %% Back to normal
  \\@separator\\\\
  \\begin{minipage}{0.40\\textwidth}
    \\hspace*{3pt}{\\@labeltext\\@projectlabel}\\\\
    \\hspace*{\\tabcolsep}\\@project
  \\end{minipage}
  \\hfill
  \\begin{minipage}{0.3\\textwidth}
    {\\@labeltext \\@datelabel}\\\\
    \\@date
  \\end{minipage}
  \\begin{minipage}{0.2\\textwidth}
    {\\@labeltext \\@durationlabel}\\\\
    \\@duration
  \\end{minipage}\\\\
  \\@separator
  \\noindent
}

\\makeatother

" ;;import de la feuille de syle dans texmf
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(defun ox-report-export-to (backend &optional async subtreep visible-only
				  body-only info)
  "Export buffer to BACKEND.
See `org-export-as' for the meaning of ASYNC SUBTREEP
VISIBLE-ONLY BODY-ONLY and INFO."
  (let* ((fname (buffer-file-name))
         (extensions '((html . ".html")
                       (latex . ".tex")
                       (ascii . ".txt")
                       (odt . ".odf")))
         (cp (point))
         (mm) ;;marker to save place
         (export-name (concat (file-name-sans-extension fname)
                              (or (cdr (assoc backend extensions)) ""))))

    (org-export-with-buffer-copy
     ;; Note I use a marker here to make sure we stay in the same place we were.
     ;; This is more robust than save-excursion I think, since processing moves
     ;; points around. In theory the marker should move too.
     (setq mm (make-marker))
     (move-marker mm cp)
     (goto-char (marker-position mm))
     (set-marker mm nil)
     (pcase backend
       ;; odt is a little bit special, and is missing one argument
       ('odt (org-open-file (org-odt-export-to-odt async subtreep visible-only
						   info)
			    'system))
       (_
	(org-open-file (org-export-to-file backend export-name
			 async subtreep visible-only
			 body-only info)
		       'system))))))


;;; ASCII Export Functions

;; Fonction pour gérer les traductions en ASCII
(defun ox-report-ascii-use-language (french english german)
  "Return appropriate translation based on document language.
FRENCH is the French text, ENGLISH the English text, GERMAN the German text."
  (let ((language (plist-get info :language)))
    (cond
     ((string-match "fr" (or language "")) french)
     ((string-match "de" (or language "")) german)
     (t english))))

(defun ox-report-ascii-template (contents info)
  "Return complete ASCII document string.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (concat
   ;; En-tête du document
   (ox-report-ascii-header info)
   "\n"
   ;; Table des participants
   (ox-report-ascii-participants-table info)
   "\n"
   ;; Contenu principal
   contents
   "\n"
   ;; Pied de page
   (ox-report-ascii-footer info)))

(defun ox-report-ascii-header (info)
  "Generate ASCII header from INFO plist."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (if (plist-get info :secretaire)
                     (plist-get info :secretaire)
                   (org-export-data (plist-get info :author) info)))
         (where (plist-get info :ou))
         (when (plist-get info :quand))
         (project (plist-get info :projet))
         (duration (plist-get info :dure))
         (initiator (plist-get info :initiateur))
         (separator (make-string 72 ?=))
         (language (or (plist-get info :language) "fr")))
    (concat
     separator "\n"
     ;; Titre centré
     (ox-report-ascii-center-string title 72) "\n"
     separator "\n\n"

     ;; Informations de la réunion
     (when where
       (concat (cond ((string-match "fr" language) "LIEU: ")
                    ((string-match "de" language) "ORT: ")
                    (t "LOCATION: "))
               where "\n"))
     (when when
       (concat (cond ((string-match "fr" language) "DATE: ")
                    ((string-match "de" language) "DATUM: ")
                    (t "DATE: "))
               when "\n"))
     (when duration
       (concat (cond ((string-match "fr" language) "DURÉE: ")
                    ((string-match "de" language) "DAUER: ")
                    (t "DURATION: "))
               duration "\n"))
     "\n"

     ;; Informations organisationnelles
     (when initiator
       (concat (cond ((string-match "fr" language) "INITIATEUR: ")
                    ((string-match "de" language) "INITIATOR: ")
                    (t "INITIATED BY: "))
               initiator "\n"))
     (when author
       (concat (cond ((string-match "fr" language) "SECRÉTAIRE: ")
                    ((string-match "de" language) "GESCHRIEBEN VON: ")
                    (t "WRITTEN BY: "))
               author "\n"))
     (when project
       (concat (cond ((string-match "fr" language) "PROJET: ")
                    ((string-match "de" language) "PROJEKT: ")
                    (t "PROJECT: "))
               project "\n"))
     "\n")))

(defun ox-report-ascii-center-string (str width)
  "Center STR in a field of WIDTH characters."
  (let* ((len (length str))
         (padding (/ (- width len) 2)))
    (if (> len width)
        str
      (concat (make-string padding ?\s) str))))

(defun ox-report-ascii-participants-table (info)
  "Generate ASCII participants table from INFO plist."
  (let ((present (plist-get info :present))
        (absent (plist-get info :absent))
        (excused (plist-get info :excuse))
        (separator (make-string 72 ?-))
        (language (or (plist-get info :language) "fr")))
    (concat
     separator "\n"
     (cond ((string-match "fr" language) "PARTICIPANTS")
          ((string-match "de" language) "TEILNEHMER")
          (t "PARTICIPANTS")) "\n"
     separator "\n\n"

     ;; Présents
     (when present
       (concat (cond ((string-match "fr" language) "PRÉSENTS:")
                    ((string-match "de" language) "ANWESEND:")
                    (t "PRESENT:")) "\n"
               (ox-report-ascii-format-participants present)
               "\n"))

     ;; Absents
     (when absent
       (concat (cond ((string-match "fr" language) "ABSENTS:")
                    ((string-match "de" language) "ABWESEND:")
                    (t "ABSENT:")) "\n"
               (ox-report-ascii-format-participants absent)
               "\n"))

     ;; Excusés
     (when excused
       (concat (cond ((string-match "fr" language) "EXCUSÉS:")
                    ((string-match "de" language) "ENTSCHULDIGT:")
                    (t "EXCUSED:")) "\n"
               (ox-report-ascii-format-participants excused)
               "\n"))

     separator "\n")))

(defun ox-report-ascii-format-participants (participants-string)
  "Format PARTICIPANTS-STRING as a bulleted list."
  (mapconcat (lambda (participant)
               (concat "  • " (string-trim participant)))
             (split-string participants-string ",")
             "\n"))

(defun ox-report-ascii-footer (info)
  "Generate ASCII footer from INFO plist."
  (let ((separator (make-string 72 ?-))
        (project (plist-get info :projet))
        (date (format-time-string "%d/%m/%Y"))
        (language (or (plist-get info :language) "fr")))
    (concat
     "\n" separator "\n"
     (format (cond ((string-match "fr" language) "Projet: %s | Date: %s")
                   ((string-match "de" language) "Projekt: %s | Datum: %s")
                   (t "Project: %s | Date: %s"))
             (or project "N/A")
             date)
     "\n" separator)))

(defun ox-report-ascii-headline (headline contents info)
  "Transcode HEADLINE element into ASCII format.
CONTENTS is the headline contents.
INFO is a plist used as a communication channel."
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         (separator (cond ((= level 1) (make-string (length title) ?=))
                         ((= level 2) (make-string (length title) ?-))
                         (t ""))))
    (concat
     "\n"
     ;; Titre de la section
     (cond ((= level 1) (upcase title))
           ((= level 2) title)
           (t (concat (make-string (* 2 (- level 1)) ?\s) "• " title)))
     "\n"
     ;; Soulignement pour les niveaux 1 et 2
     (when (and separator (not (string= separator "")))
       (concat separator "\n"))
     "\n"
     ;; Contenu
     contents)))

(defun ox-report-ascii-section (section contents info)
  "Transcode SECTION element into ASCII format.
CONTENTS is the section contents.
INFO is a plist used as a communication channel."
  contents)

;; Définition du backend ASCII dérivé
(org-export-define-derived-backend 'report-ascii 'ascii
  :options-alist
  '((:present "PRESENT" nil nil)
    (:absent "ABSENT" nil nil)
    (:excuse "EXCUSE" nil nil)
    (:secretaire "SECRETAIRE" nil nil t)
    (:secretaire "SECRETARY" nil nil t)
    (:dure "DURE" nil " ")
    (:dure "DURATION" nil " ")
    (:ou "OU" nil " ")
    (:ou "WHERE" nil " ")
    (:quand "QUAND" nil " ")
    (:quand "WHEN" nil " ")
    (:initiateur "INITIATEUR" nil " ")
    (:initiateur "INITIATOR" nil " ")
    (:projet "PROJET" nil " ")
    (:projet "PROJECT" nil " ")
    (:with-toc nil "toc" nil))
  :translate-alist
  '((template . ox-report-ascii-template)
    (headline . ox-report-ascii-headline)
    (section . ox-report-ascii-section)))

;;;###autoload
(defun ox-report-export-to-ascii (&optional async subtreep visible-only
                                           body-only info)
  "Export current buffer to a text file with report formatting.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'report-ascii outfile
      async subtreep visible-only body-only info)))

;;;###autoload
(defun ox-report-export-to-ascii-and-open (&optional async subtreep visible-only
                                                    body-only info)
  "Export current buffer to ASCII and open the file.
See `ox-report-export-to-ascii' for argument descriptions."
  (interactive)
  (let ((file (ox-report-export-to-ascii async subtreep visible-only body-only info)))
    (when file
      (org-open-file file))
    file))


;;; ODT Export Functions
;;;###autoload
(defun ox-report-export-to-odt (&optional async subtreep visible-only
                                         body-only ext-plist)
  "Export current buffer as a Report ODT file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  ;; Check if ox-odt is available
  (unless (featurep 'ox-odt)
    (user-error "ODT export backend not available. Please install ox-odt"))

  (let* ((extension ".odt")
         (file (org-export-output-file-name extension subtreep)))
    ;; Add custom content at the beginning of the buffer
    (org-export-with-buffer-copy
     (goto-char (point-min))

     ;; First, collect all the metadata
     (let (title-pos where when project duration initiator present absent excused)
       (save-excursion
         ;; Find the title position
         (when (re-search-forward "^#\\+TITLE:" nil t)
           (setq title-pos (line-end-position)))

         ;; Collect all metadata
         (goto-char (point-min))
         (when (re-search-forward "^#\\+OU:\\s-*\\(.+\\)" nil t)
           (setq where (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+QUAND:\\s-*\\(.+\\)" nil t)
           (setq when (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+PROJET:\\s-*\\(.+\\)" nil t)
           (setq project (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+DURE:\\s-*\\(.+\\)" nil t)
           (setq duration (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+INITIATEUR:\\s-*\\(.+\\)" nil t)
           (setq initiator (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+PRESENT:\\s-*\\(.+\\)" nil t)
           (setq present (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+ABSENT:\\s-*\\(.+\\)" nil t)
           (setq absent (match-string 1)))
         (goto-char (point-min))
         (when (re-search-forward "^#\\+EXCUSE:\\s-*\\(.+\\)" nil t)
           (setq excused (match-string 1))))

       ;; Insert content right after the title
       (when title-pos
         (goto-char title-pos)
         (insert "\n\n")

         ;; Insert meeting information as a table
         (insert "#+ATTR_ODT: :style \"TableWithHeading\"\n")
         (insert "| *Information* | *Détails* |\n")
         (insert "|---------------+-----------|\n")

         (when where
           (insert (format "| Lieu | %s |\n" where)))
         (when when
           (insert (format "| Date | %s |\n" when)))
         (when project
           (insert (format "| Projet | %s |\n" project)))
         (when duration
           (insert (format "| Durée | %s |\n" duration)))
         (when initiator
           (insert (format "| Initiateur | %s |\n" initiator)))

         ;; Add participants section
         (insert "\n* Participants\n\n")

         (when present
           (insert "** Présents\n")
           (dolist (person (split-string present ","))
             (insert (format "- %s\n" (string-trim person)))))

         (when absent
           (insert "\n** Absents\n")
           (dolist (person (split-string absent ","))
             (insert (format "- %s\n" (string-trim person)))))

         (when excused
           (insert "\n** Excusés\n")
           (dolist (person (split-string excused ","))
             (insert (format "- %s\n" (string-trim person)))))

         (insert "\n")))

     ;; Now export with the standard ODT exporter
     (org-odt-export-to-odt async subtreep visible-only body-only))))

;;;###autoload
(defun ox-report-export-to-odt-and-open (&optional async subtreep visible-only
                                                   body-only ext-plist)
  "Export current buffer as ODT and open.
See `ox-report-export-to-odt' for argument descriptions."
  (interactive)
  (let ((file (ox-report-export-to-odt async subtreep visible-only body-only)))
    (when file
      (org-open-file file))
    file))


;;; LaTeX/PDF Export Functions
(org-export-define-derived-backend 'report 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "report" t)
    (:present "PRESENT" nil nil)
    (:absent "ABSENT" nil nil)
    (:excuse "EXCUSE" nil nil)
    (:secretaire "SECRETAIRE" nil nil t)
    (:secretaire "SECRETARY" nil nil t)
    (:dure "DURE" nil " ")
    (:dure "DURATION" nil " ")
    (:ou "OU" nil " ")
    (:ou "WHERE" nil " ")
    (:quand "QUAND" nil " ")
    (:quand "WHEN" nil " ")
    (:initiateur "INITIATEUR" nil " ")
    (:initiateur "INITIATOR" nil " ")
    (:projet "PROJET" nil " ")
    (:projet "PROJECT" nil " ")
    (:with-toc nil "toc" 1 )
    (:latex-hyperref-p nil "texht" org-latex-with-hyperref t)
    (:resume "resume" nil nil)
    (:logo "LOGO" nil " ")
    (:style "STYLE" nil nil))
  :translate-alist '((template . ox-report-template))
  :menu-entry
  '(?R "Export to Report layout"
       ((?a "to ASCII" ox-report-export-to-ascii)
        (?A "to ASCII and open"
            (lambda (a s v b)
              (if a (ox-report-export-to-ascii-and-open t s v b)
                (ox-report-export-to-ascii-and-open nil s v b))))
        (?l "As LaTeX file" ox-report-export-to-latex)
        (?p "As PDF file" ox-report-export-to-pdf)
        (?o "As PDF and Open"
            (lambda (a s v b)
              (if a (ox-report-export-to-pdf t s v b)
                (org-open-file (ox-report-export-to-pdf nil s v b)))))
        (?d "As ODT file" ox-report-export-to-odt)
        (?D "As ODT and open"
            (lambda (a s v b)
              (if a (ox-report-export-to-odt-and-open t s v b)
                (ox-report-export-to-odt-and-open nil s v b))))
        (?m "As PDF an attach to mail"
            (lambda (a s v b)
              (if a (ox-report-export-to-pdf t s v b)
                (ox-report-pdf-to-mu4e (ox-report-export-to-pdf nil s v b))))))))

;; Properly register the backend after it's fully defined
(defun ox-report-register-backend ()
  "Register the report backend with org-export."
  (let ((backend (org-export-get-backend 'report)))
    (when backend
      (unless (memq backend org-export-registered-backends)
        (add-to-list 'org-export-registered-backends backend)))))

;; Register on load
(eval-after-load 'ox
  '(ox-report-register-backend))

;; Also try to register immediately if ox is already loaded
(when (featurep 'ox)
  (ox-report-register-backend))


(defun ox-report-pdf-to-mu4e (att)
  "Export Pdf ATT to mail."
  (when (require 'mu4e nil 'noerror)
    (mu4e--start))
  (compose-mail)
  (when att
    (if (file-exists-p att)
        (org-msg-attach-attach att)
      (message "File not found"))))

(defun ox-report-template (contents info)
  "INFO are the header data and CONTENTS is the content of the
org file and return complete document string for this export."
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Créé le %d/%m/%Y %a %H:%M \n"))
   ;; Document class and packages.
   (let* ((class (plist-get info :latex-class))
          (class-options (plist-get info :latex-class-options))
          (header (nth 1 (assoc class org-latex-classes)))
          (document-class-string
           (and (stringp header)
                (if (not class-options) header
                  (replace-regexp-in-string
                   "^[\t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
                   class-options header t nil 1)))))
     (if (not document-class-string)
         (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
        (org-latex-guess-inputenc
         (org-element-normalize-string
          (org-splice-latex-header
           document-class-string
           nil ;;org-latex-default-packages-alist ; Defined in org.el.
           org-latex-packages-alist nil    ; Defined in org.el.
           (concat (org-element-normalize-string (plist-get info :latex-header))
                   (plist-get info :latex-header-extra)))))
        info)))

   ;; Now the core content
   (let ((auteur (plist-get info :author))
         (titre (plist-get info :title)))
     (concat "

"(when (plist-get info :org-latex-with-hyperref)
   (format "{%s}" (plist-get info :org-latex-with-hyperref) ))"

\\author{"
   (if (plist-get info :secretaire)
       (format "%s" (plist-get info :secretaire))
     (org-export-data auteur info))
"}
\\title{"(org-export-data titre info)"}

\\wheremeeting{"(when (plist-get info :ou)
   (format "%s" (plist-get info :ou) )) "}
\\whenmeeting{"(when (plist-get info :quand)
   (format "%s" (plist-get info :quand) )) "}
\\initiator{"(when (plist-get info :initiateur)
   (format "%s" (plist-get info :initiateur) )) "}
\\project{"(when (plist-get info :projet)
             (format "%s" (plist-get info :projet) )) "}
\\duration{"(when (plist-get info :dure)
             (format "%s" (plist-get info :dure) )) "}

\\makeatletter
"(if (file-exists-p (plist-get info :logo))
   (format "\\newcommand{\\@mainlogo}{%s}" (plist-get info :logo))
   (when ox-report-logo
     (format "\\newcommand{\\@mainlogo}{%s}" (expand-file-name ox-report-logo)))) "
\\makeatother

"

(when (plist-get info :present)
   (mapconcat (lambda (element)
                (format "\\participant[present]{%s}" element))
              (split-string (plist-get info :present) ",")
              "\n"))

(when (plist-get info :absent)
  (mapconcat (lambda (element)
               (format "\\participant[absent]{%s}" element))
             (split-string (plist-get info :absent) ",")
             "\n"))

(when (plist-get info :excuse)
  (mapconcat (lambda (element)
               (format "\\participant[excused]{%s}" element))
             (split-string (plist-get info :excuse) ",")
             "\n"))
"
\\begin{document}
\\begin{fullpage}
\\frontmatter
"(when (plist-get info :with-toc)
   (concat
    (format "\\setcounter{tocdepth}{%d}" (plist-get info :with-toc) )
    "\\tableofcontents" ) )"
\\end{fullpage}
\\pagenumbering{arabic}
" contents "
\\singlespacing
\\end{document}
"))))


;;;###autoload
(defun ox-report-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Report (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'report outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-report-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Report (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'report file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun ox-report-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Report (pdf) and open.
If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'report outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun ox-report-setup ()
  "Setup ox-report export backend."
  (require 'ox)
  (require 'ox-latex)
  (require 'ox-ascii)
  ;; Ensure the backend is registered
  (ox-report-register-backend))

(provide 'ox-report)
;;; ox-report.el ends here
