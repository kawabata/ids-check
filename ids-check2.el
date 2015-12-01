;;; ids-check2.el --- IDS structure checker          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is further strict checking of IDS data.
;; * IDC check
;;   For "⿴⿵⿶⿷⿸⿹⿺", first DC should be limited to specific characters.
;; * DC check
;;   For "⿰⿲⿱⿳", DCs are checked against `ids-check2-dc-check-chars'.

;;; Usage
;; (ids-check2 ID IDS)

;;; Code:

(require 'ids-check)

(defconst ids-check2-illegal-dc-regexp
  (concat
   "["
   "𤣩𥫗釒牜訁龺龻𠆢"
   ""
   "]"))

(defconst ids-check2-idc-check
  `((?⿴
     ,(concat
       "井弓北占囗卩又𦥑四𠂊𠔾卝𠀃互廿㐁𠙵𠂎㔾廿㔾甴𡔲女西夂"
       "" ; extf
       ))
    (?⿵
     ,(concat
       "共癶凣𠕉𠆢𦉪⺆九冂几𠘨門戌咸戚戊齐茂凢戕门鬥齊𠔼冎乃大夙内內樊冇𠔽𠫼周𨳇丹𡘲𨳌网冈真"
       "介" ; sawndip
       ))
    (?⿶
     ,(concat
       "乂凵臼𤮺十戈弋𠃎𫠠"
       ))
    (?⿷
     ,(concat
       "丈亡仄免兑冘匚匸土尢尤工巳弓无旡氏民王石羌色虎虫見免𠥓免"
       "𠀐" ; extf
       "乇" ; sawndip
       ))
    (?⿸
     ,(concat
       "朩⺶㞑䧹丂丆严丰丿亣产十千升卩厂厃原厤厥厭厲声子尔尸尹尾局屁屵广庚府庳庶廆廌廣廱彥彦戶户戸曆最歹灰牛申疒病痲痺睂耂耳聿薦虍虎虚虛豸辰辱雁靡鴈鹿麃麻龵𠀆𠁣𠂆𠂇𠂋𠂟𠃋𠃜𠇭𠩵𠩺𡰤𢀩𢑚𢨳𤕫𥆞𨿳痳𤸰𢈙𡾷厌㾜庫"
     "⑥⑨⿻廾厚𢊆夕牙" ; ext-f
       ))
    (?⿹
     ,(concat
       "⺄㇉㦰丁丂丐丰予亐刀勹巿弋弓弔戈或武气考耳聿飞𠀁𠃌𠃍𠄎𢏚𢦏𢦑𢦒痱幾𨈐"
       "哉𠄐" ; ext-f
       ))
    (?⿺
     ,(concat
       "㇉㐬㪅䖍七丈丸久乇乙乚九也乳亢人來儿兀允元兄充兆先光兊克免兒兔冗冘几凢包叁及友反土堯夂夊夌夏大夨夷宂完尢尣尤尧尺尾屯嵬巳巴廴忒支文无旡是晃曳更木未札枚檒武歲毛气氣永洗為爪瓜瓦电疌皃礼竜竟笔篭羌羑老色虎虔處見覺见豕走起足辵辶边近述逐通速造連遇過遣霓風风鬼鳯麥麦黽鼠鼡龍龙龜﨤𠃊𠃑𠃔𠕻𠙳𠧒𣎳𣎴𤰔𧺆𪜕𫎇𫠠𫳭逌遙廷"
       "过遠芚②米⑤𫵼⿳叉凡夜" ; ext-f
       "電岜追死" ; sawndip
       ))
    )
  "Alist of IDC vs appropriate first args."
  )

(defconst ids-check2-dc-check-chars
  `((left
     "彳亻扌氵丬犭牜礻糹纟衤讠𧾷钅飠𩙿饣忄歹歺𤴔爿")
    (right
     "亍刂")
    (top
     "癶丷⼧𠆢艹艹爫㓁𦍌覀𠔉龹⺈罒亠⺊罓𦉰𦍌⽵")
    (bottom
     "龰灬朩氺𧘇𫶧𣥂")
    (not-left
     "足")
    (not-right
     "")
    (not-top
     "人亼爪")
    (not-bottom
     ""))
  "Alist of position vs appropriate characters."
  )

(defconst ids-check2-dc-check-table
  (let ((table (make-hash-table :test 'equal)))
    (dolist (locations '((not-left right)
                         (not-right left)
                         (top bottom)
                         (not-top bottom)
                         (not-bottom top)
                         (left right)))
      (puthash locations
               (mapconcat
                 (lambda (location)
                   (car (alist-get location ids-check2-dc-check-chars)))
                 locations "")
               table))
    table)
  "Cached data of warning characters."
  )

(defconst ids-check2-dc-check
  '((?⿰ (1 . (not-left right)) (2 . (not-right left))
        ((1 2) . (top bottom)))
    (?⿱ (1 . (not-top bottom)) (2 . (not-bottom top))
        ((1 2) . (left right)))
    (?⿲ (1 . (not-left right)) (3 . (not-right left))
        ((1 2 3) . (top bottom)))
    (?⿳ (1 . (not-top bottom)) (3 . (not-bottom top))
        ((1 2 3) . (left right)))))

(defvar ids-check2-id nil)
(defvar ids-check2-ids nil)

;;;###autoload
(defun ids-check2 (id ids)
  "Check ID of IDS."
  (setq ids-check2-id id
        ids-check2-ids ids)
  (when (string-match ids-check2-illegal-dc-regexp ids)
    (ids-check2-message "IDS contains illegal DC (%s)."
                        (match-string 0 ids)))
  (when (string-match "^[⿰-⿻]" ids)
    (ids-check2-tree (ids-tree-structure ids) t)))

(defun ids-check2-tree (tree &optional top)
  "Recursively check ids TREE.
If TOP is t, then normalization test will not be performed."
  (let* ((idc (car tree))
         (idc-check
          (car (alist-get idc ids-check2-idc-check)))
         (dc-check
          (alist-get idc ids-check2-dc-check))
         (dc1 (cadr tree))
         (dc2 (elt tree 2))
         (dc3 (elt tree 3))
         (norms (unless top (ids-normalize (apply 'string (-flatten tree))))))
    (dolist (norm norms)
      (when (= (length norm) 1)
        (ids-check2-message "IDS may contain unifiable component %s."
                            norm)))
    (when (and idc-check
               (characterp dc1)
               (not (string-match (char-to-string dc1) idc-check)))
      (ids-check2-message "IDC %c not match with DC %c."
                          idc dc1))
    (when dc-check
      (dolist (check dc-check)
        (let* ((pos (car check))
               (pos (if (integerp pos) (list pos) pos))
               (locations (cdr check))
               (check-chars (gethash locations ids-check2-dc-check-table)))
          (dolist (position pos)
            (when (and (characterp (elt tree position))
                       (string-match (char-to-string (elt tree position))
                                     check-chars))
              (ids-check2-message
               "IDS may contain inappropriately positioned DC (%s)."
               (match-string 0 check-chars)))))))
    (when (or (= idc ?⿳) (= idc ?⿲))
      (ids-check2-tertiary-idc idc dc1 dc2 dc3))
    (unless (characterp dc1) (ids-check2-tree dc1))
    (unless (characterp dc2) (ids-check2-tree dc2))
    (unless (or (null dc3) (characterp dc3)) (ids-check2-tree dc3))))

(defun ids-check2-tertiary-idc (idc dc1 dc2 dc3)
  "Check unifiability of IDC first/last two args (DC1 DC2 and DC3)."
  (let* ((idc2 (if (= idc ?⿲) ?⿰ ?⿱))
         (norm1 (ids-normalize
                 (apply 'string (-flatten (list idc2 dc1 dc2)))))
         (norm2 (ids-normalize
                 (apply 'string (-flatten (list idc2 dc2 dc3))))))
    (dolist (norm (nconc norm1 norm2))
      (when (= (length norm) 1)
        (ids-check2-message "IDS may contain unifiable component %s."
                            norm)))))

(defun ids-check2-message (format &rest args)
  "Display message FORMAT and ARGS for ‘ids-check2’ module."
  (apply 'message
         (concat "* %s (%s): " format)
         ids-check2-id ids-check2-ids args))

(provide 'ids-check2)
;;; ids-check2.el ends here
