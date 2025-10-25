export const scrollIntoView_ = id => {
  if (typeof window === "undefined") return

  document.getElementById(id).scrollIntoView({ behavior: "instant" })
}

export const truthy_ = x => !!x

export const infinity = Number.POSITIVE_INFINITY
