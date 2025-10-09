const ctx = new AudioContext()

export const playNote_ = (durationMs, wave, freq) => {
  const duration = durationMs / 1000
  const osc = ctx.createOscillator()
  const gain = ctx.createGain()
  const attack = 0.01
  const release = 0.05
  const now = ctx.currentTime
  const minGain = 0.001
  const maxGain =
    wave === "square" ? 0.08
    : wave === "sawtooth" ? 0.13
    : wave === "triangle" ? 0.24
    : 0.25

  osc.frequency.value = freq
  osc.type = wave

  gain.gain.setValueAtTime(minGain, now)
  gain.gain.exponentialRampToValueAtTime(maxGain, now + attack)
  gain.gain.setValueAtTime(maxGain, now + duration - release)
  gain.gain.exponentialRampToValueAtTime(minGain, now + duration)

  osc.connect(gain).connect(ctx.destination)
  osc.start(now)
  osc.stop(now + duration)
}
