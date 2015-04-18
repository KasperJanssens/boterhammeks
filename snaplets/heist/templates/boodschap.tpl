<apply template="base">
  <dfForm action="/boodschap">
    <dfChildErrorList ref="" />

    <dfLabel ref="boodschapper">Boodschapper: </dfLabel>
    <dfInputText ref="boodschapper" />
    <br>

    <dfLabel ref="bedrag">Bedrag: </dfLabel>
    <dfInputText ref="bedrag" />
    <br>

    <dfLabel ref="etendecollegae">Etende Collegae: </dfLabel>
    <dfInputText ref="etendecollegae" />
    <br>

    <dfInputSelectGroup ref="meerderecollegae">
      <option>Plan1
      <option>Plan2
      <option>Plan3
    </dfInputSelectGroup>

    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>