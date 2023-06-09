package io.github.ghostbuster91.sttp.client3.openapi

package object raw {
  // type ReferenceOr[C[_], T[_]] =
  //   Either[RReference[C], ({ type X[A] = T[C[A]] })#X]

  type GReferenceOr[C[_], T] = Either[GReference[C], T]

  type Id[A] = A
}
