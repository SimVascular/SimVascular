/* Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 2006 Stanford University.
 * Constants were taken from public sources such as 
 * itkGE5ImageIO.h within the ITK package.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including 
 * without limitation the rights to use, copy, modify, merge, publish, 
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _CV_IMG_OFFSETS_H_
#define _CV_IMG_OFFSETS_H_

#define OFFSET_MAGIC_NUM	0
#define OFFSET_HEADER_LEN	0x4
#define OFFSET_IMAGE_WIDTH	0x8
#define OFFSET_IMAGE_HEIGHT	0xc
#define OFFSET_SUITE		0x7c
#define OFFSET_EXAM			0x84
#define OFFSET_SERIES		0x8c
#define OFFSET_IMAGE		0x94

#define IM_VAS_COLLAPSE 	0x24a 	/* 586 <COLLAPSE IMAGE */
#define IM_VENC 			0x240 	/* 576 <PHASE CONTRAST VELOCITY ENCODING */

#define IM_VASFLAGS 		0x2da 	/* 730 */
#define IM_VENCSCALE 		0x2dc 	/* 732 */

#define EX_NO 				0x8 	/* 8 <EXAM NUMBER */

#define EX_PATID			0x54	/* 84 <PATIENT ID FOR THIS EXAM */
#define EX_PATNAME			0x61	/* 97 <PATIENT NAME */
#define EX_TYP				0x131	/* 305 <EXAM TYPE */

#define IM_SENO 			0xa 	/* 10 <SERIES NUMBER FOR THIS IMAGE */
#define IM_NO 				0xc 	/* 12 <IMAGE NUMBER */
#define IM_ACTUAL_DT 		0x12 	/* 18 <ACTUAL IMAGE DATE/TIME STAMP */

#define IM_SLTHICK 			0x1a 	/* 26 <SLICE THICKNESS (MM) */

#define IM_IMATRIX_X 		0x1e 	/* 30 <IMAGE MATRIX SIZE - X */
#define IM_IMATRIX_Y 		0x20 	/* 32 <IMAGE MATRIX SIZE - Y */

#define IM_DIM_X 	0x2a 	/* 42 <IMAGE DIMENSION - X */
#define IM_DIM_Y 	0x2e 	/* 46 <IMAGE DIMENSION - Y */

#define IM_PIXSIZE_X 	0x32 	/* 50 <IMAGE PIXEL SIZE - X */
#define IM_PIXSIZE_Y 	0x36 	/* 54 <IMAGE PIXEL SIZE - Y */

#define IM_NORM_R 	0x8e 	/* 142 <NORMAL R COORD */
#define IM_NORM_A 	0x92 	/* 146 <NORMAL A COORD */
#define IM_NORM_S 	0x96 	/* 150 <NORMAL S COORD */

#define IM_TLHC_R 	0x9a 	/* 154 <R COORD OF TOP LEFT HAND CORNER */
#define IM_TLHC_A 	0x9e 	/* 158 <A COORD OF TOP LEFT HAND CORNER */
#define IM_TLHC_S 	0xa2 	/* 162 <S COORD OF TOP LEFT HAND CORNER */

#define IM_TRHC_R 	0xa6 	/* 166 <R COORD OF TOP LEFT HAND CORNER */
#define IM_TRHC_A 	0xaa 	/* 170 <A COORD OF TOP LEFT HAND CORNER */
#define IM_TRHC_S 	0xae 	/* 174 <S COORD OF TOP LEFT HAND CORNER */

#define IM_BRHC_R 	0xb2 	/* 178 <R COORD OF TOP LEFT HAND CORNER */
#define IM_BRHC_A 	0xb6 	/* 182 <A COORD OF TOP LEFT HAND CORNER */
#define IM_BRHC_S 	0xba 	/* 186 <S COORD OF TOP LEFT HAND CORNER */

#define IM_HRTRATE 	0xe0 	/* 224 <CARDIAC HEART RATE (BPM) */

#define IM_PSDNAME 	0x134 	/* 308 <PULSE SEQUENCE NAME */

/* User Variables */

#define IM_USER2 	0x19e 	/* 414 */
#define IM_USER5 	0x1aa 	/* 426 */
#define IM_USER6 	0x1ae 	/* 430 */
#define IM_USER7 	0x1b2 	/* 434 */
#define IM_USER8 	0x1b6 	/* 438 */
#define IM_USER9 	0x1ba 	/* 442 */
#define IM_USER12 	0x1c6 	/* 454 */
#define IM_USER13 	0x1ca 	/* 458 */
#define IM_USER14 	0x1ce 	/* 462 */

#endif 

