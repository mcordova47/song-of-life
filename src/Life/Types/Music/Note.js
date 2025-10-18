let ctx = null
let reverb = null

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
  const randRelease = Math.random() * release
  const decay = release - randRelease

  osc.frequency.value = freq
  osc.type = wave

  gain.connect(reverb.input.dry)
  gain.connect(reverb.input.wetIn)

  gain.gain.setValueAtTime(minGain, now)
  gain.gain.exponentialRampToValueAtTime(pitchAdjustedGain, now + attack)
  gain.gain.setValueAtTime(pitchAdjustedGain, now + duration - randRelease)
  gain.gain.exponentialRampToValueAtTime(minGain, now + duration + decay)

  osc.connect(gain)
  osc.start(now)
  osc.stop(now + duration)
}

export const drone_ = (wave, freq) => {
  const osc = ctx.createOscillator()
  const gain = ctx.createGain()
  const attack = 0.01
  const release = 0.05
  const now = ctx.currentTime
  const minGain = 0.001
  const maxGain = 0.05
  const pitchAdjustedGain = loudnessCompensation(freq) * maxGain

  osc.frequency.value = freq
  osc.type = wave

  gain.connect(reverb.input.dry)
  gain.connect(reverb.input.wetIn)

  gain.gain.setValueAtTime(minGain, now)
  gain.gain.exponentialRampToValueAtTime(pitchAdjustedGain, now + attack)

  osc.connect(gain)
  osc.start(now)

  return {
    stop() {
      gain.gain.exponentialRampToValueAtTime(minGain, ctx.currentTime)
      osc.stop(ctx.currentTime + release)
    }
  }
}

const loudnessCompensation = freq => {
  const minFreq = 50
  const maxFreq = 5000
  const norm = Math.log(freq / minFreq) / Math.log(maxFreq / minFreq)
  const dB = 6 * (1 - norm) - 3 * norm
  return Math.pow(10, dB / 20)
}

const makeReverbChain = () => {
  const delay = ctx.createDelay()
  delay.delayTime.value = 0.1

  const feedback = ctx.createGain()
  feedback.gain.value = 0.2

  const tone = ctx.createBiquadFilter()
  tone.type = 'highpass'
  tone.frequency.value = 400

  const wet = ctx.createGain()
  wet.gain.value = 0.15

  const dry = ctx.createGain()
  dry.gain.value = 0.7

  delay.connect(feedback)
  feedback.connect(tone)
  tone.connect(delay)
  delay.connect(wet)
  wet.connect(ctx.destination)
  dry.connect(ctx.destination)

  return {
    input: { dry, wetIn: tone },
    delay, feedback, tone, wet, dry,
  }
}

if (typeof window !== "undefined") {
  ctx = new AudioContext()
  reverb = makeReverbChain()
}
