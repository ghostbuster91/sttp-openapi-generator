package com.example

object Main {
  def main(args: Array[String]): Unit =
    new DefaultApi("http://example.com").getRoot()
}
