export const playNotes_ = (notes, duration) => {
  notes.forEach((note) => playNote(note, duration))
}

const ctx = new AudioContext()

export const ensureAudio = () => {
  if (ctx.state === "suspended") {
    ctx.resume()
  }
}

const playNote = (freq, durationMs) => {
  const duration = durationMs / 1000
  const osc = ctx.createOscillator()
  const gain = ctx.createGain()
  const attack = 0.01
  const release = 0.05
  const now = ctx.currentTime

  osc.frequency.value = freq
  osc.type = "sine"

  // gain.gain.setValueAtTime(0, now);
  // gain.gain.linearRampToValueAtTime(0.2, now + attack)
  // gain.gain.setValueAtTime(0.2, now + duration - release)
  // gain.gain.linearRampToValueAtTime(0, now + duration)

  gain.gain.setValueAtTime(0.001, now)
  gain.gain.exponentialRampToValueAtTime(0.2, now + attack)
  gain.gain.setValueAtTime(0.2, now + duration - release)
  gain.gain.exponentialRampToValueAtTime(0.001, now + duration)

  osc.connect(gain).connect(ctx.destination)
  osc.start(now)
  osc.stop(now + duration)
}
