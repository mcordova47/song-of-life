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

export const dice_ = props =>
  svg(
    props,
    path(`
      M5 3H3v18h18V3H5z
      m14 2v14H5V5h14z
      M9 7H7v2h2V7zm6 0h2v2h-2V7z
      m-6 8H7v2h2v-2z
      m6 0h2v2h-2v-2z
      m-2-4h-2v2h2v-2z
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

// Custom

export const share_ = props =>
  svg(
    { ...props, viewBox: "0 0 24 24" },
    path(`
      M7 9v-2h2v-2h2v-2h2v2h2v2h2v2h-2v-2h-2v-2h-2v2h-2v2h-2
      M11 5v13h2V5
      M5 13v9h14v-9h-2v7H7v-7H5z
      M9 11v2h-4v-2z
      M15 11v2h4v-2z
    `)
  )

export const sineWave_ = props =>
  svg(
    { ...props, viewBox: "0 0 17 17" },
    path(`
      M1 9
      v-2h1v-2h1v-1h1v-1h2
      v1h1v1h1v2h1v3h1v2h1v1h2
      v-1h1v-2h1v-2h1
      v2h-1v2h-1v1h-1v1h-2
      v-1h-1v-1h-1v-2h-1v-3h-1v-2h-1v-1h-2
      v1h-1v2h-1v2h-1z
    `)
  )

export const squareWave_ = props =>
  svg(
    { ...props, viewBox: "0 0 17 17" },
    path(`
      M1 9
      v-5h8
      v8h6
      v-4h1
      v5h-8
      v-8h-6
      v4z
    `)
  )

export const triangleWave_ = props =>
  svg(
    { ...props, viewBox: "0 0 17 17" },
    path(`
      M2 9
      v-1h1v-1h1v-1h1v-1h1
      v1h1v1h1v1h1v1h1v1h1v1h1
      v-1h1v-1h1v-1h1
      v1h-1v1h-1v1h-1v1h-1
      v-1h-1v-1h-1v-1h-1v-1h-1v-1h-1v-1h-1
      v1h-1v1h-1v1h-1z
    `)
  )

export const sawtoothWave_ = props =>
  svg(
    { ...props, viewBox: "0 0 17 17" },
    path(`
      M1 9
      v-1h2v-1h2v-1h2v-1h2
      v6h1
      v-1h2v-1h2v-1h2
      v1h-2v1h-2v1h-2v1h-2
      v-6h-1
      v1h-2v1h-2v1h-2z
    `)
  )

export const logo_ = props =>
  svg(
    { ...props, viewBox: "0 0 100 100"},
    path(`
      M25 15v4h4v-4z
      M30 15v4h4v-4z
      M35 15v4h4v-4z
      M40 15v4h4v-4z
      M45 15v4h4v-4z
      M50 15v4h4v-4z
      M55 15v4h4v-4z
      M60 15v4h4v-4z
      M65 15v4h4v-4z
      M70 15v4h4v-4z
      M20 20v4h4v-4z
      M75 20v4h4v-4z
      M20 25v4h4v-4z
      M75 25v4h4v-4z
      M20 30v4h4v-4z
      M75 30v4h4v-4z
      M20 35v4h4v-4z
      M75 35v4h4v-4z
      M20 40v4h4v-4z
      M75 40v4h4v-4z
      M20 45v4h4v-4z
      M40 45v4h4v-4z
      M45 45v4h4v-4z
      M75 45v4h4v-4z
      M20 50v4h4v-4z
      M40 50v4h4v-4z
      M45 50v4h4v-4z
      M60 50v4h4v-4z
      M75 50v4h4v-4z
      M5 55v4h4v-4z
      M10 55v4h4v-4z
      M15 55v4h4v-4z
      M20 55v4h4v-4z
      M55 55v4h4v-4z
      M65 55v4h4v-4z
      M75 55v4h4v-4z
      M80 55v4h4v-4z
      M85 55v4h4v-4z
      M90 55v4h4v-4z
      M0 60v4h4v-4z
      M20 60v4h4v-4z
      M30 60v4h4v-4z
      M35 60v4h4v-4z
      M40 60v4h4v-4z
      M55 60v4h4v-4z
      M65 60v4h4v-4z
      M75 60v4h4v-4z
      M95 60v4h4v-4z
      M0 65v4h4v-4z
      M20 65v4h4v-4z
      M35 65v4h4v-4z
      M40 65v4h4v-4z
      M45 65v4h4v-4z
      M60 65v4h4v-4z
      M75 65v4h4v-4z
      M95 65v4h4v-4z
      M0 70v4h4v-4z
      M20 70v4h4v-4z
      M75 70v4h4v-4z
      M95 70v4h4v-4z
      M0 75v4h4v-4z
      M20 75v4h4v-4z
      M35 75v4h4v-4z
      M65 75v4h4v-4z
      M75 75v4h4v-4z
      M95 75v4h4v-4z
      M5 80v4h4v-4z
      M10 80v4h4v-4z
      M15 80v4h4v-4z
      M20 80v4h4v-4z
      M30 80v4h4v-4z
      M40 80v4h4v-4z
      M55 80v4h4v-4z
      M65 80v4h4v-4z
      M75 80v4h4v-4z
      M80 80v4h4v-4z
      M85 80v4h4v-4z
      M90 80v4h4v-4z
      M30 85v4h4v-4z
      M35 85v4h4v-4z
      M60 85v4h4v-4z
      M65 85v4h4v-4z
    `)
  )

// Utils

const svg = (props, children) => e(
  "svg",
  {
    xmlns: "http://www.w3.org/2000/svg",
    viewBox: props.viewBox || "0 0 24 24",
    width: props.size,
    height: props.size
  },
  children
)

const path = d => path_({ fill: "currentColor", d })

const path_ = props => e("path", props)
