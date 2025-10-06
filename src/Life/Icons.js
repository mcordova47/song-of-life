// SVGs from https://pixelarticons.com/
import React from "react"

const e = React.createElement

export const arrowBarRight_ = props =>
  svg(
    props,
    path(`
      M18
      4v16h2V4h-2zM4
      11v2h8v2h-2v2h2v-2h2v-2h2v-2h-2V9h-2V7h-2v2h2v2H4z
    `)
  )

export const pause_ = props =>
  svg(
    props,
    path(`
      M10
      4H5v16h5V4zm9
      0h-5v16h5V4z
    `)
  )

export const play_ = props =>
  svg(
    props,
    path(`
      M10
      20H8V4h2v2h2v3h2v2h2v2h-2v2h-2v3h-2v2z
    `)
  )

export const externalLink_ = props =>
  svg(
    props,
    path(`
      M21
      11V3h-8v2h4v2h-2v2h-2v2h-2v2H9v2h2v-2h2v-2h2V9h2V7h2v4h2zM11
      5H3v16h16v-8h-2v6H5V7h6V5z
    `)
  )

export const github_ = props =>
  svg(
    props,
    path(`
      M5
      2h4v2H7v2H5V2Zm0
      10H3V6h2v6Zm2
      2H5v-2h2v2Zm2
      2v-2H7v2H3v-2H1v2h2v2h4v4h2v-4h2v-2H9Zm0
      0v2H7v-2h2Zm6-12v2H9V4h6Zm4
      2h-2V4h-2V2h4v4Zm0
      6V6h2v6h-2Zm-2 2v-2h2v2h-2Zm-2
      2v-2h2v2h-2Zm0
      2h-2v-2h2v2Zm0
      0h2v4h-2v-4Z
    `)
  )

export const trash_ = props =>
  svg(
    props,
    path(`
      M16
      2v4h6v2h-2v14H4V8H2V6h6V2h8zm-2
      2h-4v2h4V4zm0
      4H6v12h12V8h-4zm-5
      2h2v8H9v-8zm6
      0h-2v8h2v-8z
    `)
  )

const svg = (props, children) => e(
  "svg",
  {
    xmlns: "http://www.w3.org/2000/svg",
    viewBox: "0 0 24 24",
    width: props.size,
    height: props.size
  },
  children
)

const path = d => e(
  "path",
  {
    fill: "currentColor",
    d
  }
)
