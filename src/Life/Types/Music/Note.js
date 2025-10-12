const ctx = typeof window === "undefined" ? null : new AudioContext()

export const play_ = (durationMs, wave, freq) => {
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
  const pitchAdjustedGain = loudnessCompensation(freq) * maxGain

  osc.frequency.value = freq
  osc.type = wave

  gain.gain.setValueAtTime(minGain, now)
  gain.gain.exponentialRampToValueAtTime(pitchAdjustedGain, now + attack)
  gain.gain.setValueAtTime(pitchAdjustedGain, now + duration - release)
  gain.gain.exponentialRampToValueAtTime(minGain, now + duration)

  osc.connect(gain).connect(ctx.destination)
  osc.start(now)
  osc.stop(now + duration)
}

const loudnessCompensation = freq => {
  const minFreq = 50
  const maxFreq = 5000
  const norm = Math.log(freq / minFreq) / Math.log(maxFreq / minFreq)
  const dB = 6 * (1 - norm) - 3 * norm
  return Math.pow(10, dB / 20)
}
