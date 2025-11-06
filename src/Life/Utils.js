export const scrollIntoView_ = id => {
  if (typeof window === "undefined") return

  const el = document.getElementById(id)
  if (!el) return

  el.scrollIntoView({ behavior: "instant" })
}

export const truthy_ = x => !!x
