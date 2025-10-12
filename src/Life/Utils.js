export const scrollIntoView_ = id => {
  if (typeof window === "undefined") return

  document.getElementById(id).scrollIntoView({ behavior: "instant" })
}
