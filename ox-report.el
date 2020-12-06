;;; ox-report.el --- Export your org file to minutes report PDF file -*- lexical-binding: t -*-

;; Copyright (C) 2020  Matthias David
;; Author: Matthias David <matthias@gnu.re>
;; URL: https://github.com/DarkBuffalo/ox-report
;; Version: 0.2
;; Package-Requires: ((emacs "24.4"))
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
(require 'mu4e)


(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex")))

(add-to-list 'org-latex-classes
             '("report"                          ;class-name
               "\\documentclass[10pt]{article}

\\usepackage[defaultfam,light,tabular,lining]{montserrat} %% Option 'defaultfam'
%% only if the base font of the document is to be sans serig
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{Montserrat-TOsF}\\selectfont #1}}

\\RequirePackage[utf8]{inputenc}
\\RequirePackage[T1]{fontenc}

\\RequirePackage{setspace}              %%pour le titre
\\RequirePackage{graphicx}	        %% gestion des images
\\RequirePackage[dvipsnames,table]{xcolor}	%% gestion des couleurs
\\RequirePackage{array}		%% gestion améliorée des tableaux
\\RequirePackage{calc}		        %% syntaxe naturelle pour les calculs
\\RequirePackage{enumitem}	        %% pour les listes numérotées
\\RequirePackage[footnote]{snotez}	%% placer les notes de pied de page sur le coté
\\RequirePackage{microtype,textcase}
\\RequirePackage{titlesec}
\\RequirePackage{booktabs}

\\RequirePackage{amsmath,
	amssymb,
	amsthm} 			%% For including math equations, theorems, symbols, etc
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
    {\\skip\\noindent\\begin{minipage}
    {\\textwidth+\\marginparwidth+\\marginparsep}\\skip\\smallskip}
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

%% Command to provide alternative translations in French and English
\\newcommand{\\FrenchEnglish}[2]{
   \\iflanguage{french}{#1}{}
   \\iflanguage{english}{#2}{}}

%% This} separating line is used across several documents,
\\newcommand{\\@separator}{%%
 %% To make sure we have spacing on both sides, make an invisible rule, 2X tall
  \\rule{0ex}{2ex}%%
   %% Place the dashed rule 1X high
  \\textcolor{mdgray}{\\rule[1ex]{\\textwidth}{0.25pt}}%%
}


%% LABEL %<-------------------------------------------------------->%
%% Standard style for labels, small and bold
\\newcommand{\\@labeltext}{\\large\\scshape}

\\newcommand*{\\@approvedlabel}{\\FrenchEnglish{APPROUVE PAR}{APPROVED BY}}
\\newcommand*{\\@approved}{Set with \\texttt{\textbackslash approved\\{\\}}}
\\newcommand*{\\approved}{\\renewcommand*{\\@approved}}

\\newcommand*{\\@authorlabel}{\\FrenchEnglish{Auteur(s)}{Author(s)}}
\\newcommand*{\\@Authorlabel}{\\FrenchEnglish{AUTEUR(S)}{AUTHOR(S)}}

\\newcommand*{\\@checkedlabel}{\\FrenchEnglish{VERIFIE PAR}{CHECKED BY}}
\\newcommand*{\\@checked}{Set with \\texttt{\textbackslash checked\\{\\}}}
\\newcommand*{\\checked}{\\renewcommand*{\\@checked}}

\\newcommand*{\\@datelabel}{\\FrenchEnglish{DATE}{DATE}}
\\newcommand*{\\@absentlabel}{\\FrenchEnglish{ABSENT}{ABSENT}}
\\newcommand*{\\@excusedlabel}{\\FrenchEnglish{EXCUSE}{EXCUSED}}

\\newcommand*{\\@durationlabel}{\\FrenchEnglish{DUREE}{DURATION}}
\\newcommand*{\\@duration}{Set with \\texttt{\\textbackslash duration\\{\\}}}
\\newcommand*{\\duration}{\\renewcommand*{\\@duration}}

\\newcommand*{\\@initiatorlabel}{\\FrenchEnglish{INITIATEUR}{INITIATED BY}}
\\newcommand*{\\@initiator}{Set with \\texttt{\\textbackslash initiator\\{\\}}}
\\newcommand*{\\initiator}{\\renewcommand*{\\@initiator}}

\\newcommand*{\\@participantlabel}{\\FrenchEnglish{PARTICIPANT}{PARTICIPANT}}
\\newcommand*{\\@participantslabel}{\\FrenchEnglish{PARTICIPANTS}{PARTICIPANTS}}

\\newcommand*{\\@preparedlabel}{\\FrenchEnglish{PREPARE PAR}{PREPARED BY}}
\\newcommand*{\\@prepared}{Set with \\texttt{\\textbackslash prepared\\{\\}}}
\\newcommand*{\\prepared}{\\renewcommand*{\\@prepared}}

\\newcommand*{\\@presentlabel}{\\FrenchEnglish{PRESENT}{PRESENT}}

\\newcommand*{\\@projectlabel}{\\FrenchEnglish{PROJET}{PROJECT}}
\\newcommand*{\\@project}{Set with \\texttt{\\textbackslash project\\{\\}}}
\\newcommand*{\\project}{\\renewcommand*{\\@project}}

\\newcommand*{\\@referencelabel}{\\FrenchEnglish{REFERENCE}{REFERENCE}}

\\newcommand*{\\@reportlabel}{\\FrenchEnglish{Rapport}{Report}}

\\newcommand*{\\@reportnumberlabel}{\\FrenchEnglish{RAPPORT N°}{REPORT NUMBER}}
\\newcommand*{\\@reportnumber}{Set with \\texttt{\\textbackslash reportnumber\\{\\}}}
\\newcommand*{\\reportnumber}{\\renewcommand*{\\@reportnumber}}

\\newcommand*{\\@wheremeeting}{Set with \\texttt{\\textbackslash wheremeeting\\{\\}}}
\\newcommand*{\\wheremeeting}{\\renewcommand*{\\@wheremeeting}}

\\newcommand*{\\@whenmeeting}{Set with \\texttt{\\textbackslash whenmeeting\\{\\}}}
\\newcommand*{\\whenmeeting}{\\renewcommand*{\\@whenmeeting}}


%% TASKS

\\newcommand*{\\@tasklistlabel}{\\FrenchEnglish{Liste de tâches}{Task List}}
\\newcommand*{\\@tasknumberlabel}{\\#}
\\newcommand*{\\@tasklabel}{\\FrenchEnglish{TACHE}{TASK}}

\\newcommand*{\\@duelabel}{\\FrenchEnglish{DATE D'ECHEANCE}{DUE DATE}}

\\newcommand*{\\@responsiblelabel}{\\FrenchEnglish{RESPONSABLE}{RESPONSIBLE}}


%% MINUTES %<------------------------------------------------------->%
\\ProcessOptions\\relax

\\PassOptionsToPackage{table}{xcolor}

\\renewcommand*{\\@authorlabel}{\\FrenchEnglish{ECRIT PAR}{WRITTEN BY}}


%% Setting up header and footer
\\RequirePackage{nccfancyhdr,lastpage}
\\pagestyle{fancy}

%% Header
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
  \\thepage\\ \\FrenchEnglish{de}{of} \\pageref{LastPage}
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
\\RequirePackage{colortbl,tabularx,setspace,rotating}
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
               ("\\subsection{%s}" . "\\subsection*a{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defgroup org-export-report nil
  "Options specific to Report back-end."
  :tag "Org Report PDF"
  :group 'ox-report)


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
  '(?r "Export to Report layout"
       ((?l "As LaTeX file" ox-report-export-to-latex)
        (?p "As PDF file" ox-report-export-to-pdf)
        (?o "As PDF and Open"
            (lambda (a s v b)
              (if a (ox-report-export-to-pdf t s v b)
                (org-open-file (ox-report-export-to-pdf nil s v b)))))
        (?m "As PDF an attach to mail"
            (lambda (a s v b)
              (if a (ox-fart-export-to-pdf t s v b)
             (ox-fart-pdf-to-mu4e (ox-fart-export-to-pdf nil s v b))
                ))))))


(defun ox-fart-pdf-to-mu4e (att)
  "Export Pdf ATT to mail."
  (mu4e~start)
  (mu4e-compose-new)
  (when att
    (if (file-exists-p att)
        (org-msg-attach-attach att)
      (mu4e-warn "File not found"))))

(defun ox-report-template (contents info)
  "INFO are the header data and CONTENTS is the content of the org file and return complete document string for this export."
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
           org-latex-default-packages-alist ; Defined in org.el.
           org-latex-packages-alist nil     ; Defined in org.el.
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
       (format "%s" (plist-get info :secretaire) )
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
"(when (plist-get info :logo)
   (format "\\newcommand{\\@mainlogo}{%s}" (plist-get info :logo) )) "
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

(provide 'ox-report)
;;; ox-report.el ends here
