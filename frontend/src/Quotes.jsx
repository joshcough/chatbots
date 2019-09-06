import React from "react"
import { Dropdown, Table } from "react-bootstrap"

export const View = props =>
  <div>

    <Dropdown>
      <Dropdown.Toggle variant="Secondary" id="streams">
        Streams
      </Dropdown.Toggle>

      <Dropdown.Menu>
        {props.streams.map((s, _) =>
          <Dropdown.Item active={props.stream==s} eventKey={s} onSelect={(k, _) => props.selectStream(k)}>{s}</Dropdown.Item>
        )}
      </Dropdown.Menu>
    </Dropdown>

     <Table striped bordered hover variant="dark">
       <thead>
         <tr>
           <th>Channel</th>
           <th>Quote Id</th>
           <th>Quote</th>
         </tr>
       </thead>
       <tbody>
          {props.quotes.map((q, _) =>
            <tr>
              <td>{q.channel}</td>
              <td>{q.qid}</td>
              <td>{q.body}</td>
            </tr>
           )}
       </tbody>
     </Table>
  </div>
