clear all

matrix input C = (1000\5000)
matrix input A = (0.999, 0.05\0.001, 0.95)

matrix list C
matrix list A

di A[1,1] + A[2,1] // should be 1
di A[1,2] + A[2,2] // should be 1
di C[1,1] + C[2,1] // should be 6000


*******************************************
** ITERATION THROUGH ACRES
*******************************************
/* ONE MULTIPLICATION */
matrix AC = A*C
matrix list AC
di AC[1,1] + AC[2,1] // should be 6000

/* TEN THOUSAND MULTIPLICATIONS */
local iter = 1

while `iter' < 100000 {
	local iter = `iter' + 1
	matrix AC = A*AC
}

di `iter'

matrix list AC
di AC[1,1] + AC[2,1] // should be 6000


*******************************************
** ITERATION THROUGH PROBABILITIES
*******************************************
/* ONE MULTIPLICATION */
matrix AA = A*A
matrix list AA
di AA[1,1] + AA[2,1] // should be 1
di AA[1,2] + AA[2,2] // should be 1

/* TEN THOUSAND MULTIPLICATIONS */
local iter = 1

while `iter' < 100000 {
	local iter = `iter' + 1
	matrix AA = A*AA
}

di `iter'

matrix list AA
di AA[1,1] + AA[2,1] // should be 1
di AA[1,2] + AA[2,2] // should be 1


*******************************************
** MANUALLY CALCULATE PROBABILITIES
*******************************************
/* ONE MULTIPLICATION */
matrix AA2 = (A[1,1]*A[1,1]+A[1,2]*A[2,1], ///
			  A[1,1]*A[1,2]+A[1,2]*A[2,2]\ ///
			  A[2,1]*A[1,1]+A[2,2]*A[2,1], ///
			  A[2,1]*A[1,2]+A[2,2]*A[2,2])

matrix list AA2
di AA2[1,1] + AA2[2,1] // should be 1
di AA2[1,2] + AA2[2,2] // should be 1

/* TEN THOUSAND MULTIPLICATIONS */
local iter = 1

while `iter' < 100000 {
	local iter = `iter' + 1
	matrix AA2 = (A[1,1]*AA2[1,1]+A[1,2]*AA2[2,1], ///
				  A[1,1]*AA2[1,2]+A[1,2]*AA2[2,2]\ ///
				  A[2,1]*AA2[1,1]+A[2,2]*AA2[2,1], ///
				  A[2,1]*AA2[1,2]+A[2,2]*AA2[2,2])
}

di `iter'

matrix list AA2
di AA2[1,1] + AA2[2,1] // should be 1
di AA2[1,2] + AA2[2,2] // should be 1


*******************************************
** EXAMPLE OF ONE COUNTY
*******************************************
// As long as the row sum does not equal to 1, the iteration would not work.

matrix input A0 = (.9999966, .02609782\3.397e-06, .97390217)
matrix list A0

di A0[1,1] + A0[2,1] // should be 1
di A0[1,2] + A0[2,2] // should be 1

/* ONE MULTIPLICATION */
matrix AA0 = A0*A0
matrix list AA0
di AA0[1,1] + AA0[2,1] // should be 1
di AA0[1,2] + AA0[2,2] // should be 1

/* TEN THOUSAND MULTIPLICATIONS */
local iter = 1

while `iter' < 100000 {
	local iter = `iter' + 1
	matrix AA0 = A0*AA0
}

di `iter'

matrix list AA0
di AA0[1,1] + AA0[2,1] // should be 1
di AA0[1,2] + AA0[2,2] // should be 1




















