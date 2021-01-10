'use strict'

function hexToRgba(hex) {
  if(hex[0] == '#') {
    return hexToRgba(hex.substring(1))
  }
  else {
    const hexes = [
      hex.substring(0, 2),
      hex.substring(2, 4),
      hex.substring(4, 6),
    ]
    const [r, g, b] = hexes.map(h => parseInt(h, 16))
    return `rgba(${r}, ${g}, ${b}, 1)`
  }
}

/**
 * Here's the colors taken from
 * https://github.com/nashamri/spacemacs-theme/blob/master/spacemacs-common.el

| name          | color   |
|---------------+---------|
| act1          | #222226 |
| act2          | #5d4d7a |
| base          | #b2b2b2 |
| base-dim      | #686868 |
| bg1           | #292b2e |
| bg2           | #212026 |
| bg3           | #100a14 |
| bg4           | #0a0814 |
| border        | #5d4d7a |
| cblk          | #cbc1d5 |
| cblk-bg       | #2f2b33 |
| cblk-ln       | #827591 |
| cblk-ln-bg    | #373040 |
| cursor        | #e3dedd |
| const         | #a45bad |
| comment       | #2aa1ae |
| comment-bg    | #292e34 |
| comp          | #c56ec3 |
| err           | #e0211d |
| func          | #bc6ec5 |
| head1         | #4f97d7 |
| head1-bg      | #293239 |
| head2         | #2d9574 |
| head2-bg      | #293235 |
| head3         | #67b11d |
| head3-bg      | #293235 |
| head4         | #b1951d |
| head4-bg      | #32322c |
| highlight     | #444155 |
| highlight-dim | #3b314d |
| keyword       | #4f97d7 |
| lnum          | #44505c |
| mat           | #86dc2f |
| meta          | #9f8766 |
| str           | #2d9574 |
| suc           | #86dc2f |
| ttip          | #9a9aba |
| ttip-sl       | #5e5079 |
| ttip-bg       | #34323e |
| type          | #ce537a |
| var           | #7590db |
| war           | #dc752f |
| aqua          | #2d9574 |
| aqua-bg       | #293235 |
| green         | #67b11d |
| green-bg      | #293235 |
| green-bg-s    | #29422d |
| cyan          | #28def0 |
| red           | #f2241f |
| red-bg        | #3c2a2c |
| red-bg-s      | #512e31 |
| blue          | #4f97d7 |
| blue-bg       | #293239 |
| magenta       | #a31db1 |
| yellow        | #b1951d |
| yellow-bg     | #32322c |
 */

const buttonColor = '#2aa1ae'

// TODO: Lift the variable translation. Just put them in a :root that applies to
// everything. We'll loose things like hexToRgba but it doesn't see much use.
const vars = {
  '--badge-border': '1px solid #5d4d7a',
  '--badge-background-color': '#100a14',
  '--badge-border': '#dc752f',
  // Emacs calls this "war" and it's typically used for callouts on headings
  // such as [1/3] and [50%] that are shown from TODO check lists. Probably
  // because "var" is taken and "w" comes after "v".
  '--badge-color': '#dc752f',
  '--border': '#5d4d7a',
  '--border-color': '#5d4d7a',
  '--button-background': '#1a3834',
  '--button-background-color': '#1a3834',
  '--button-border': '2px solid #2aa1ae',
  '--button-disabled-background-color': '#212026',
  '--button-disabled-border': '2px solid #686868',
  '--button-disabled-color': '#686868',
  '--button-color': '#2aa1ae',
  '--button-color-rgba': hexToRgba(buttonColor),
  '--button-hover-color':  '#6ae1ee',
  '--button-hover-border': '2px solid #6ae1ee',
  '--button-hover-background-color': '#205050',
  '--callout-color': '#e3dedd',
  '--callout-background-color': '#5d4d7a',
  '--code-json-string-color': '#a45bad ',
  'code-string-interpolation': '#86dc2f',
  '--code-gutter-background-color': '#212026',
  '--code-gutter-color': '#686868',
  '--code-gutter-border-color': '#5d4d7a',
  '--disabled-color': '#686868',
  '--error-color': '#ce537a',
  '--error-background-color': '#3c2a2c',
  '--func-color': '#bc6ec5',
  '--keyword-color': '#4f97d7',
  '--text': '#b2b2b2',
  '--text-color': '#b2b2b2',
  '--dim-text': '#686868',
  '--background': '#292b2e',
  '--general-background-color': '#292b2e',
  '--general-background': '#292b2e',
  '--code-background-color': '#100a14',
  '--code-color': '#b0b0d0',
  '--link-color': '#2d9574',
  '--link-hover-color': '#4b9',
  '--link-visited-color': '#4f97d7',
  '--link-visited-background-color': '#44505c',
  '--hover-link-color': '#4b9', // Duplicate.
  '--visited-link-color': '#4f97d7', // Duplicate.
  '--visited-link-background-color': '#44505c', // Duplicate.
  '--heading-background-color': '#292b2e',
  '--head1-color': '#4f97d7',
  '--head1-background-color': '#293239',
  '--head2-color': '#2d9574',
  '--head2-background-color': '#293235',
  '--head3-color': '#67b11d',
  '--head3-background-color': '#293235',
  '--head4-color': '#b1951d',
  '--head4-background-color': '#32322c',
  '--highlight-dim-color': '#e3dedd',
  '--highlight-dim-background-color': '#3b314d',
  '--highlight-background-color': '#444155',
  'highlight-color': '#e3dedd',
  // This is the same as the code background color. Perhaps we should require
  // code blocks use something a little brighter?
  // TODO: Rename instances of this to gutter-*.
  'line-number-background-color': '#100a14',
  'line-number-color': '#686868',
  'gutter-background-color': '#100a14',
  'gutter-color': '#686868',
  '--meta-background-color': '#541',
  '--meta-color': '#b1951d',
  '--separator-background-color': '#125',
  '--separator-color': '#78d',
  '--tab-inactive-color': '#2aa1ae',
  '--tab-inactive-background-color': '#244',
  '--tab-left-inactive-border-radius': '0 0 0.5em 0',
  '--tab-right-inactive-border-radius': '0 0 0 0.5em',
  // Setting the container's background-color makes it so we get the S shaped
  // border radius on the tabs, because the border-radius on the bottom allows
  // the background-color of the container to leak through.
  '--tab-inactive-sibling-container-background-color': '#424',
  '--tab-active-background-color': '#424',
  '--tab-active-border-radius': '0.5em 0.5em 0 0',
  '--tab-active-border': '1px solid #75d',
  '--tab-active-color': '#75d',
  '--tab-list-background-color': '#292b2e',
  '--table-header-color': '#79e',
  '--table-header-background-color': '#235',
  '--table-row-alternate-background-color': '#212026',
  '--table-row-hover-background-color': '#383038',
  '--info-background-color': '#060',
  '--info-color': '#90e030',
  '--input-color': '#2aa1ae',
  '--input-placeholder-color': '#116162',
  '--input-background-color': '#292e34',
  'input-border': '1px solid #2aa1ae',
  '--input-label-color': '#2aa1ae',
  /**
   * Active items are usually the ones important under a context. They will
   * typically be whatever the "selected" thing is.
   */
  '--item-active-color': '#e3dedd',
  '--item-active-background-color': '#5d4d7a',
  /**
   * A composite like this doesn't seem to work - at least in Firefox. I thought
   * I witnessed this working in Safari.
   */
  '--thin-border': '1px solid #5d4d7a',
  '--thin-border-color': '#5d4d7a',
  /**
   * The standard Spacemacs diff colors are a bit harsh. I intend to update them
   * in the theme as well.
   */
  // '--diff-add-color': '#293235',
  // '--diff-add-background-color': '#86dc2f',
  // '--diff-minor-add-background-color': '#67b11d',
  '--diff-major-add-color': '#86dc2f',
  '--diff-major-add-background-color': '#184818',
  '--diff-minor-add-color': '#67b11d',
  '--diff-minor-add-background-color': '#030',
  '--diff-major-remove-background-color': '#3c2a2c',
  '--diff-major-remove-color': '#e0616d',
  '--diff-minor-remove-background-color': '#311',
  '--diff-minor-remove-color': '#ce537a',
  '--open-ticket-color': '#86dc2f',
  '--closed-ticket-color': '#ce537a',
  '--fancy-color': '#97d',
  '--fancy-background-color': '#536',
  '--calendar-leave-entry-background': '#29422d',
  '--calendar-leave-entry-color': '#67b11d',
  '--panel-border': '0.1em solid #5d4d7a',
  '--panel-title-color': '#b2b2b2',
  '--panel-title-background': '#5d4d7a',
  '--panel-title-background-color': '#5d4d7a',
  '--panel-title-padding': '1em',
  'panel-background-color': '#0a0814', // Uses "bg4" from color list.
  '--time-slice': '#2d9574',
  '--time-slice-background': '#29422d',
  '--time-slice-inactive-background': '#100a14',
  '--time-slice-cursor-background': '#5d4d7a',
  '--warning-background': '#441',
  '--warning-background-color': '#441',
  '--warning-border': '1px solid #dc2',
  '--warning-color': '#dc2',
}

module.exports = vars
